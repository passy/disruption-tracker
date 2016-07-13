{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Lib
import qualified Citymapper.Types         as C
import qualified Options.Applicative      as Opt
import qualified Network.Wreq             as Wreq

import           Control.Applicative      ((<**>))
import           Control.Lens             ((^.), (^..), traverse, _Just)
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
      resp <- Wreq.asJSON =<< Wreq.get Lib.disruptionUrl
      -- Need to pack this as a tuple of line name + disruptions. Maybe ID?
      let routes = resp
               ^.. Wreq.responseBody
                 . C.groupings
                 . traverse
                 . C.routes
                 . _Just
                 . traverse
                 . C.status
                 . C.disruptions
                 . traverse
      print routes
