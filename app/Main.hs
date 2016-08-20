{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Main where

import qualified Control.Exception.Lifted    as E
import qualified Control.Monad.Reader        as Reader
import qualified Data.Default                as Default
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.IO           as TLIO
import qualified Data.Text.Read              as Read
import qualified Lib
import qualified Lib.Citymapper.Types        as C
import qualified Lib.DB
import qualified Network.Wreq                as Wreq
import qualified Options.Applicative         as Opt
import qualified Options.Applicative.Text    as OptT
import qualified Options.Applicative.Types   as Opt

import           Control.Applicative         (optional, (<**>))
import           Control.Concurrent          (threadDelay)
import           Control.Lens                (mapped, over, traverse, (^.),
                                              (^..), _Just)
import           Control.Monad               (forever, void)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Monoid                 ((<>))
import           Data.Version                (Version (), showVersion)
import           Paths_disruption_tracker    (version)
import           System.Environment          (getProgName)

data Verbosity = Normal | Verbose
  deriving (Eq, Show)

data Options = Options
  { optHostname  :: T.Text
  , optPort      :: Integer
  , optPassword  :: Maybe T.Text
  , optVerbosity :: Verbosity
  , optCommand   :: Command
  } deriving (Show)

type OptT = Reader.ReaderT Options IO ()

runOptT :: OptT -> Options -> IO ()
runOptT = Reader.runReaderT

instance Default.Default Options where
  def =
    Options "localhost" 28015 Default.def Normal NoOp

data Command = Collect
             | CollectD Int
             | Setup
             | NoOp
  deriving (Show)

options :: Opt.Parser Options
options =
  Options <$> OptT.textOption ( Opt.long "hostname"
                             <> Opt.short 'h'
                             <> Opt.help "RethinkDB hostname"
                             <> Opt.value (optHostname Default.def)
                             <> Opt.showDefault )
          <*> integerOption ( Opt.long "port"
                           <> Opt.short 'p'
                           <> Opt.help "RethinkDB port"
                           <> Opt.value (optPort Default.def)
                           <> Opt.showDefault )
          <*> optional ( OptT.textOption ( Opt.long "password"
                                        <> Opt.short 'w'
                                        <> Opt.help "RethinkDB password" ) )
          <*> Opt.flag Normal Verbose
             ( Opt.long "verbose"
            <> Opt.short 'v'
            <> Opt.help "Provide status updates on every invocation" )
          <*> command

collectDOptions :: Opt.Parser Command
collectDOptions =
  CollectD <$> intOption ( Opt.long "interval"
                        <> Opt.short 'i'
                        <> Opt.help "Interval to check for new disruptions at (seconds)"
                        <> Opt.value 5
                        <> Opt.showDefault )

command :: Opt.Parser Command
command =
  Opt.subparser (
     Opt.command "setup" ( Opt.info (pure Setup) (Opt.progDesc "One-time database setup. Run before collect.") )
  <> Opt.command "collect" ( Opt.info (pure Collect) (Opt.progDesc "Run a one-time collection.") )
  <> Opt.command "collectd" ( Opt.info collectDOptions (Opt.progDesc "Start a continous collection. (Not actually daemonizing.)") ) )

integer :: Opt.ReadM Integer
integer = either error fst . Read.decimal . T.pack <$> Opt.readerAsk

int :: Opt.ReadM Int
int = either error fst . Read.decimal . T.pack <$> Opt.readerAsk

integerOption :: Opt.Mod Opt.OptionFields Integer -> Opt.Parser Integer
integerOption = Opt.option integer

intOption :: Opt.Mod Opt.OptionFields Int -> Opt.Parser Int
intOption = Opt.option int

cliParser :: String -> Version -> Opt.ParserInfo Options
cliParser progName ver =
  Opt.info ( Opt.helper <*> options <**> versionInfo )
    ( Opt.fullDesc
   <> Opt.progDesc "Record Tube disruptions"
   <> Opt.header progName )
  where
    versionInfo = Opt.infoOption ( unwords [progName, showVersion ver] )
      ( Opt.short 'V'
     <> Opt.long "version"
     <> Opt.hidden
     <> Opt.help "Show version information" )

main :: IO ()
main = do
  progName <- getProgName
  Opt.execParser (cliParser progName version) >>= runOptT run
  where
    host :: Options -> Lib.DB.Host
    host opts = Lib.DB.Host (optHostname opts) (optPort opts) (optPassword opts)

    run :: OptT
    run = Reader.asks optCommand >>= \case
      Setup -> runSetup
      Collect -> runCollect
      CollectD interval -> loopIndefinitely interval runCollect
      NoOp -> error "Invalid command."

    runSetup :: OptT
    runSetup = Reader.asks host >>= liftIO . Lib.DB.setup

    loopIndefinitely :: forall m a. (Reader.MonadIO m, MonadBaseControl IO m) => Int -> m a -> m a
    loopIndefinitely seconds fn =
      forever $ do
        _ <- void fn `E.catch` handler
        liftIO . threadDelay $ seconds * 1000 * 1000
      where
        handler :: forall m. Reader.MonadIO m => E.SomeException -> m ()
        handler e = liftIO . putStrLn $ "Ignored Error: " <> show e

    runCollect :: OptT
    runCollect = do
      resp <- liftIO $ Wreq.asJSON =<< Wreq.get (T.unpack Lib.disruptionUrl)
      let routes :: [C.Route]
          routes = resp
               ^.. Wreq.responseBody
                 . C.groupings
                 . traverse
                 . C.routes
                 . _Just
                 . traverse
      let extrLine r =  Lib.DB.LinesRow (r ^. C.routeName)
                                        (r ^. C.status . C.description)
                                        (r ^. C.status . C.statusLevel)
                                        (r ^.. C.status . C.disruptions . traverse)
      let disruptions = over mapped extrLine routes
      h <- Reader.asks host
      results <- liftIO . sequence $ Lib.DB.writeDisruptions h <$> disruptions
      printSummary $ Lib.summarizeWriteResponse results

    printSummary :: Maybe TL.Text -> OptT
    printSummary text = Reader.asks optVerbosity >>= \v ->
      liftIO $ case (v, text) of
        (Normal, Nothing) ->  pure ()
        (Verbose, Nothing) -> TLIO.putStrLn "No changes."
        (_, Just t) -> TLIO.putStrLn t
