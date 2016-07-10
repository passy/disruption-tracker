{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import qualified Network.Wreq             as Wreq
import qualified Options.Applicative      as Opt
import qualified Data.Aeson.Types as Aeson

import           Control.Applicative      ((<**>))
import           Control.Lens             ((^.))
import           Data.Monoid              ((<>))
import           Data.Version             (Version (), showVersion)
import           GHC.Generics             (Generic)
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

disruptionUrl :: String
disruptionUrl = "https://citymapper.com/api/1/routestatus?weekend=0"

data RouteStatusResponse = RouteStatusResponse
  { last_updated_time :: T.Text
  , groupings         :: [RouteStatus]
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON RouteStatusResponse

data RouteStatus = RouteStatus
  { name :: T.Text
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON RouteStatus

main :: IO ()
main = do
  progName <- getProgName
  Opt.execParser (cliParser progName version) >>= run
  where
    run :: Options -> IO ()
    run _ = do
      r <- Wreq.asJSON =<< Wreq.get disruptionUrl
      print (r ^. Wreq.responseBody :: RouteStatusResponse)
