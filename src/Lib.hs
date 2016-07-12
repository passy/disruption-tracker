{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib where

import Control.Monad (void)

import qualified Control.Lens.TH          as L
import qualified Data.Aeson.Casing        as AesonC
import qualified Data.Aeson.Types         as Aeson
import qualified Data.Text                as T
import qualified Database.RethinkDB       as R
import qualified GHC.Generics             as Generics

disruptionUrl :: String
disruptionUrl = "https://citymapper.com/api/1/routestatus?weekend=0"

labelModifier :: String -> String
labelModifier = AesonC.snakeCase . drop 1

aesonOptions :: Aeson.Options
aesonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = labelModifier }

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
  , _stops             :: Maybe [T.Text]
  , _disruptionLevel   :: Int
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

routeDisruptionNameModifier :: String -> String
routeDisruptionNameModifier "_disruptionSummary" = "summary"
routeDisruptionNameModifier "_disruptionLevel" = "level"
routeDisruptionNameModifier s = labelModifier s

instance Aeson.FromJSON RouteDisruption where
  parseJSON = Aeson.genericParseJSON $ aesonOptions { Aeson.fieldLabelModifier = routeDisruptionNameModifier }

instance Aeson.ToJSON RouteDisruption where
  toEncoding = Aeson.genericToEncoding $ aesonOptions { Aeson.fieldLabelModifier = routeDisruptionNameModifier }

$(L.makeLenses ''RouteDisruption)

data RouteStatus = RouteStatus
  { _statusSummary :: T.Text
  , _description   :: T.Text
  , _statusLevel   :: Int
  , _disruptions   :: [RouteDisruption]
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

routeStatusNameModifier :: String -> String
routeStatusNameModifier "_statusSummary" = "summary"
routeStatusNameModifier "_statusLevel" = "level"
routeStatusNameModifier s = labelModifier s

instance Aeson.FromJSON RouteStatus where
  parseJSON = Aeson.genericParseJSON $ aesonOptions { Aeson.fieldLabelModifier = routeStatusNameModifier }

instance Aeson.ToJSON RouteStatus where
  toEncoding = Aeson.genericToEncoding $ aesonOptions { Aeson.fieldLabelModifier = routeStatusNameModifier }

$(L.makeLenses ''RouteStatus)

data Route = Route
  { _routeName :: T.Text
  , _status    :: RouteStatus
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

routeNameModifier :: String -> String
routeNameModifier "_routeName" = "name"
routeNameModifier s = labelModifier s

instance Aeson.FromJSON Route where
  parseJSON = Aeson.genericParseJSON $ aesonOptions { Aeson.fieldLabelModifier = routeNameModifier }

instance Aeson.ToJSON Route where
  toEncoding = Aeson.genericToEncoding $ aesonOptions { Aeson.fieldLabelModifier = routeNameModifier }

$(L.makeLenses ''Route)

data Grouping = Grouping
  { _groupingName :: T.Text
  , _id           :: T.Text
  , _routes       :: Maybe [Route]
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

groupingNameModifier :: String -> String
groupingNameModifier "_groupingName" = "name"
groupingNameModifier s = labelModifier s

instance Aeson.FromJSON Grouping where
  parseJSON = Aeson.genericParseJSON $ aesonOptions { Aeson.fieldLabelModifier = groupingNameModifier }

instance Aeson.ToJSON Grouping where
  toEncoding = Aeson.genericToEncoding $ aesonOptions { Aeson.fieldLabelModifier = groupingNameModifier }

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

disruptionsTable :: R.Table
disruptionsTable = R.table "disruptions"

writeRecord :: RouteStatus -> IO ()
writeRecord s = do
  h <- R.connect "192.168.99.100" 32772 Nothing
  void . R.run' h $ R.tableCreate disruptionsTable
  (void . R.run' h) $ R.insert s disruptionsTable
