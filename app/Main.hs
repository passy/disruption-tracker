{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import qualified Control.Lens.TH          as L
import qualified Data.Aeson.Casing        as AesonC
import qualified Data.Aeson.Types         as Aeson
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import qualified Database.RethinkDB       as R
import qualified GHC.Generics             as Generics
import qualified Network.Wreq             as Wreq
import qualified Options.Applicative      as Opt

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

disruptionUrl :: String
disruptionUrl = "https://citymapper.com/api/1/routestatus?weekend=0"

aesonOptions :: Aeson.Options
aesonOptions = Aeson.defaultOptions { Aeson.fieldLabelModifier = AesonC.snakeCase . drop 1 }

genericParse
  :: (Generics.Generic a, Aeson.GFromJSON (Generics.Rep a))
  => Aeson.Value
  -> Aeson.Parser a
genericParse = Aeson.genericParseJSON aesonOptions

genericToEncoding
  :: (Generics.Generic a, Aeson.GToEncoding (Generics.Rep a))
  => a
  -> Aeson.Encoding
genericToEncoding = Aeson.genericToEncoding aesonOptions

data RouteDisruption = RouteDisruption
  { _disruptionSummary :: T.Text
  , _stops   :: Maybe [T.Text]
  , _disruptionLevel   :: Int
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

instance Aeson.FromJSON RouteDisruption where
  parseJSON = genericParse

instance Aeson.ToJSON RouteDisruption where
  toEncoding = genericToEncoding

$(L.makeLenses ''RouteDisruption)

data RouteStatus = RouteStatus
  { _statusSummary :: T.Text
  , _description :: T.Text
  , _statusLevel :: Int
  , _disruptions :: [RouteDisruption]
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

instance Aeson.FromJSON RouteStatus where
  parseJSON = genericParse

instance Aeson.ToJSON RouteStatus where
  toEncoding = genericToEncoding

$(L.makeLenses ''RouteStatus)

data Route = Route
  { _name   :: T.Text
  , _status :: RouteStatus
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

instance Aeson.FromJSON Route where
  parseJSON = genericParse

instance Aeson.ToJSON Route where
  toEncoding = genericToEncoding

$(L.makeLenses ''Route)

data Grouping = Grouping
  { _groupingName   :: T.Text
  , _id     :: T.Text
  , _routes :: Maybe [Route]
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

instance Aeson.FromJSON Grouping where
  parseJSON = genericParse

instance Aeson.ToJSON Grouping where
  toEncoding = genericToEncoding

$(L.makeLenses ''Grouping)

data RouteStatusResponse = RouteStatusResponse
  { _lastUpdatedTime :: T.Text
  , _groupings       :: [Grouping]
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

instance Aeson.FromJSON RouteStatusResponse where
  parseJSON = genericParse

instance Aeson.ToJSON RouteStatusResponse where
  toEncoding = genericToEncoding

$(L.makeLenses ''RouteStatusResponse)

main :: IO ()
main = do
  progName <- getProgName
  Opt.execParser (cliParser progName version) >>= run
  where
    run :: Options -> IO ()
    run _ = do
      r <- Wreq.asJSON =<< Wreq.get disruptionUrl
      print (r ^. Wreq.responseBody :: RouteStatusResponse)

disruptionsTable :: R.Table
disruptionsTable = R.table "disruptions"

writeRecord :: RouteStatus -> IO ()
writeRecord s = do
  h <- R.connect "localhost" 32772 Nothing
  void . R.run' h $ R.tableCreate disruptionsTable
  (void . R.run' h) $ R.insert s disruptionsTable
