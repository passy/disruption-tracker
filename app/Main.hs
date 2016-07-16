{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.Text                as T
import qualified GHC.Generics             as Generics
import qualified Lib
import qualified Lib.Citymapper.Types     as C
import qualified Lib.DB
import qualified Network.Wreq             as Wreq
import qualified Options.Applicative      as Opt
import qualified Options.Generic

import           Control.Applicative      ((<**>))
import           Control.Lens             (mapped, over, traverse, (^.), (^..),
                                           _Just)
import           Data.Monoid              ((<>))
import           Data.Version             (Version (), showVersion)
import           Paths_disruption_tracker (version)
import           System.Environment       (getProgName)

data Options = Options
  { optHostname :: T.Text
  , optPort     :: Integer
  , optPassword :: Maybe T.Text
  } deriving (Generics.Generic, Show, Eq, Options.Generic.ParseRecord)

options :: Opt.Parser Options
options = Options.Generic.parseRecord

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
    run opts = do
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
      let host = Lib.DB.Host (optHostname opts) (optPort opts) (optPassword opts)
      sequence_ $ Lib.DB.writeDisruptions host . toDisruptionsRow <$> disruptions

    toDisruptionsRow :: (T.Text, [C.RouteDisruption]) -> Lib.DB.DisruptionRow
    toDisruptionsRow (name, disruptions) = Lib.DB.DisruptionRow { .. }
