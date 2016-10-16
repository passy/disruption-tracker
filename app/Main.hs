{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude hiding (threadDelay, (<>))

import qualified Control.Exception.Safe as E
import qualified Control.Monad.Reader as Reader
import qualified Data.Default as Default
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Read as Read
import qualified Lib
import qualified Lib.Citymapper.Types as C
import qualified Lib.DB
import qualified Network.Wreq as Wreq
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Text as OptT
import qualified Options.Applicative.Types as Opt
import qualified System.IO as IO

import Control.Applicative (optional, (<**>))
import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Lens (mapped, over, traverse, (^.), (^..), _Just)
import Control.Monad (forever, void)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Data.Version (Version(), showVersion)
import Paths_disruption_tracker (version)
import System.Environment (getProgName)
import Control.Monad.Trans.Control (MonadBaseControl)

data Verbosity
  = Normal
  | Verbose
  deriving (Eq, Show)

data DieOnError
  = SwallowError
  | DieOnError
  deriving (Eq, Show)

data Options = Options
  { optHostname :: T.Text
  , optPort :: Integer
  , optPassword :: Maybe T.Text
  , optVerbosity :: Verbosity
  , optCommand :: Command
  } deriving (Show)

type OptT = Reader.ReaderT Options IO ()

runOptT :: OptT -> Options -> IO ()
runOptT = Reader.runReaderT

instance Default.Default Options where
  def = Options "localhost" 28015 Default.def Normal NoOp

data Command
  = Collect
  | CollectD Int
             DieOnError
  | Setup
  | NoOp
  deriving (Show)

options :: Opt.Parser Options
options =
  Options <$>
  OptT.textOption
    (Opt.long "hostname" <> Opt.short 'h' <> Opt.help "RethinkDB hostname" <>
     Opt.value (optHostname Default.def) <>
     Opt.showDefault) <*>
  integerOption
    (Opt.long "port" <> Opt.short 'p' <> Opt.help "RethinkDB port" <>
     Opt.value (optPort Default.def) <>
     Opt.showDefault) <*>
  optional
    (OptT.textOption
       (Opt.long "password" <> Opt.short 'w' <> Opt.help "RethinkDB password")) <*>
  Opt.flag
    Normal
    Verbose
    (Opt.long "verbose" <> Opt.short 'v' <>
     Opt.help "Provide status updates on every invocation") <*>
  command

collectDOptions :: Opt.Parser Command
collectDOptions =
  CollectD <$>
  intOption
    (Opt.long "interval" <> Opt.short 'i' <>
     Opt.help "Interval to check for new disruptions at (seconds)" <>
     Opt.value 5 <>
     Opt.showDefault) <*>
  Opt.flag
    SwallowError
    DieOnError
    (Opt.long "die-on-error" <> Opt.short 'e' <>
     Opt.help "Exit on error instead of ignoring it")

command :: Opt.Parser Command
command =
  Opt.subparser
    (Opt.command
       "setup"
       (Opt.info
          (pure Setup)
          (Opt.progDesc "One-time database setup. Run before collect.")) <>
     Opt.command
       "collect"
       (Opt.info (pure Collect) (Opt.progDesc "Run a one-time collection.")) <>
     Opt.command
       "collectd"
       (Opt.info
          collectDOptions
          (Opt.progDesc
             "Start a continous collection. (Not actually daemonizing.)")))

integral
  :: Integral a
  => Opt.ReadM a
integral = either error fst . first T.pack . Read.decimal . T.pack <$> Opt.readerAsk

integer :: Opt.ReadM Integer
integer = integral

int :: Opt.ReadM Int
int = integral

integerOption :: Opt.Mod Opt.OptionFields Integer -> Opt.Parser Integer
integerOption = Opt.option integer

intOption :: Opt.Mod Opt.OptionFields Int -> Opt.Parser Int
intOption = Opt.option int

cliParser :: Text -> Version -> Opt.ParserInfo Options
cliParser progName ver =
  Opt.info
    (Opt.helper <*> options <**> versionInfo)
    (Opt.fullDesc <> Opt.progDesc "Record Tube disruptions" <>
     Opt.header (T.unpack progName))
  where
    versionInfo =
      Opt.infoOption
        (T.unpack (T.unwords [progName, T.pack (showVersion ver)]))
        (Opt.short 'V' <> Opt.long "version" <> Opt.hidden <>
         Opt.help "Show version information")

main :: IO ()
main = do
  progName <- T.pack <$> getProgName
  Opt.execParser (cliParser progName version) >>= runOptT run
  where
    host :: Options -> Lib.DB.Host
    host opts = Lib.DB.Host (optHostname opts) (optPort opts) (optPassword opts)
    run :: OptT
    run =
      Reader.asks optCommand >>=
      \case
        Setup -> runSetup
        Collect -> runCollect
        CollectD interval SwallowError -> loopIndefinitelySilent interval runCollect
        CollectD interval DieOnError -> loopIndefinitely interval runCollect
        NoOp -> error "Invalid command."
    runSetup :: OptT
    runSetup = Reader.asks host >>= liftIO . Lib.DB.setup
    loopIndefinitely
      :: forall m.
         (Reader.MonadIO m, MonadBaseControl IO m)
      => Int -> m () -> m ()
    loopIndefinitely seconds fn =
      forever $
      do fn
         threadDelay $ seconds * 1000 * 1000
    loopIndefinitelySilent
      :: forall m.
         (E.MonadCatch m, Reader.MonadIO m, MonadBaseControl IO m)
      => Int -> m () -> m ()
    loopIndefinitelySilent seconds fn =
      forever $
      do _ <- fork $ worker fn
         threadDelay $ seconds * 1000 * 1000
      where
        handler
          :: forall m.
             Reader.MonadIO m
          => E.SomeException -> m ()
        handler e =
          liftIO . IO.hPutStrLn IO.stderr $ "Ignored Error: " <> show e
        worker
          :: forall m a.
             (Reader.MonadIO m, E.MonadCatch m)
          => m a -> m ()
        worker fn' = void fn' `E.catchAny` handler
    runCollect :: OptT
    runCollect = do
      resp <- liftIO $ Wreq.asJSON =<< Wreq.get (T.unpack Lib.disruptionUrl)
      let routes :: [C.Route]
          routes =
            resp ^.. Wreq.responseBody . C.groupings . traverse . C.routes . _Just .
            traverse
      let timestamp :: C.JSONDateTime
          timestamp = resp ^. Wreq.responseBody . C.lastUpdatedTime
      h <- Reader.asks host
      results <- liftIO . sequence $ Lib.DB.writeDisruptions h timestamp <$> routes
      printSummary $ Lib.summarizeWriteResponse results
    printSummary :: Maybe TL.Text -> OptT
    printSummary text =
      Reader.asks optVerbosity >>=
      \v ->
         liftIO $
         case (v, text) of
           (Normal, Nothing) -> pure ()
           (Verbose, Nothing) -> TLIO.putStrLn "No changes."
           (_, Just t) -> TLIO.putStrLn t
