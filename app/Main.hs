{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Options.Applicative      as Opt
import qualified Data.Aeson.Types         as Aeson

import           Control.Applicative      ((<**>))
import           Control.Lens             (( # ), (^.))
import           Control.Monad            (forM_, void)
import           Data.Monoid              ((<>))
import           Data.Version             (Version (), showVersion)
import           Paths_disruption_tracker (version)
import           System.Environment       (getProgName)

data Options = Options

options :: Opt.Parser Options
options = pure Options

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
    run :: Options -> IO ()
    run _ = do
      -- r <- Wreq.asJSON =<< Wreq.get disruptionUrl
      -- print (r ^. Wreq.responseBody :: RouteStatusResponse)

      let d = RouteDisruption { _disruptionSummary = "Something bad"
                              , _stops = Nothing
                              , _disruptionLevel = 3
                              }
      let s = RouteStatus { _statusSummary = "It's down."
                          , _description   = "I mean, it's real bad."
                          , _statusLevel   = 3
                          , _disruptions   = [d]
                        }

      -- writeRecord s
      print $ Aeson.toJSON s
