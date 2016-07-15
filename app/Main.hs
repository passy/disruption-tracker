{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.Text                as T
import qualified Lib
import qualified Lib.Citymapper.Types     as C
import qualified Lib.DB
import qualified Network.Wreq             as Wreq
import qualified Options.Applicative      as Opt

import           Control.Applicative      ((<**>))
import           Control.Lens             (mapped, over, traverse, (^.), (^..),
                                           _Just)
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
      resp <- Wreq.asJSON =<< Wreq.get (T.unpack Lib.disruptionUrl)
      let routes :: [C.Route]
          routes = resp
               ^.. Wreq.responseBody
                 . C.groupings
                 . traverse
                 . C.routes
                 . _Just
                 . traverse
      let extrDisruptions r = (r ^. C.routeName, r ^.. C.status . C.disruptions . traverse)
      let disruptions = over mapped extrDisruptions routes
      sequence_ $ Lib.DB.writeDisruptions . toDisruptionsRow <$> disruptions

    toDisruptionsRow :: (T.Text, [C.RouteDisruption]) -> Lib.DB.DisruptionRow
    toDisruptionsRow (name, disruptions) = Lib.DB.DisruptionRow { .. }
