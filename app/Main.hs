{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Default              as Default
import qualified Data.Text                 as T
import qualified Data.Text.Lazy.IO         as TLIO
import qualified Data.Text.Read            as Read
import qualified Lib
import qualified Lib.Citymapper.Types      as C
import qualified Lib.DB
import qualified Network.Wreq              as Wreq
import qualified Options.Applicative       as Opt
import qualified Options.Applicative.Text  as OptT
import qualified Options.Applicative.Types as Opt
import qualified Control.Exception         as E

import           Control.Applicative       (optional, (<**>))
import           Control.Lens              (mapped, over, traverse, (^.), (^..),
                                            _Just)
import           Control.Monad             (void, forever)
import           Data.Monoid               ((<>))
import           Data.Version              (Version (), showVersion)
import           Paths_disruption_tracker  (version)
import           System.Environment        (getProgName)
import           Control.Concurrent        (threadDelay)

data Verbosity = Normal | Verbose
  deriving (Eq, Show)

data Options = Options
  { optHostname :: T.Text
  , optPort     :: Integer
  , optPassword :: Maybe T.Text
  , optVerbosity :: Verbosity
  , optCommand  :: Command
  } deriving (Show)

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
  Opt.execParser (cliParser progName version) >>= run
  where
    host :: Options -> Lib.DB.Host
    host opts = Lib.DB.Host (optHostname opts) (optPort opts) (optPassword opts)

    run :: Options -> IO ()
    run opts = case optCommand opts of
      Setup -> runSetup opts
      Collect -> runCollect opts
      CollectD interval -> loopIndefinitely interval . E.uninterruptibleMask_ $ runCollect opts
      NoOp -> error "Invalid command."

    runSetup :: Options -> IO ()
    runSetup opts =
      void . Lib.DB.setup $ host opts

    loopIndefinitely :: Int -> IO () -> IO ()
    loopIndefinitely seconds fn =
      forever $ fn >> threadDelay (seconds * 1000 * 1000)

    runCollect :: Options -> IO ()
    runCollect opts = do
      resp <- Wreq.asJSON =<< Wreq.get (T.unpack Lib.disruptionUrl)
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
      results <- sequence $ Lib.DB.writeDisruptions (host opts) <$> disruptions
      maybe (pure ()) TLIO.putStrLn $ Lib.summarizeWriteResponse results
