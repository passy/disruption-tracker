{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Citymapper.Types where

import qualified Control.Lens.TH    as L
import qualified Data.Aeson.Casing  as AesonC
import qualified Data.Aeson.Types   as Aeson
import qualified Data.Text          as T
import qualified Database.RethinkDB as R
import qualified GHC.Generics       as Generics

defaultModifier :: String -> String
defaultModifier = AesonC.snakeCase . drop 1

genericParseJSON
  :: (Generics.Generic a, Aeson.GFromJSON (Generics.Rep a))
  => (String -> String)
  -> Aeson.Value
  -> Aeson.Parser a
genericParseJSON modifier = Aeson.genericParseJSON Aeson.defaultOptions
  { Aeson.fieldLabelModifier = modifier }

genericToEncoding
  :: (Generics.Generic a, Aeson.GToEncoding (Generics.Rep a))
  => (String -> String)
  -> a
  -> Aeson.Encoding
genericToEncoding modifier = Aeson.genericToEncoding Aeson.defaultOptions
  { Aeson.fieldLabelModifier = modifier }

genericToJSON
  :: (Generics.Generic a, Aeson.GToJSON (Generics.Rep a))
  => (String -> String)
  -> a
  -> Aeson.Value
genericToJSON modifier = Aeson.genericToJSON Aeson.defaultOptions
  { Aeson.fieldLabelModifier = modifier }

data RouteDisruption = RouteDisruption
  { _disruptionSummary :: T.Text
  , _stops             :: Maybe [T.Text]
  , _disruptionLevel   :: Int
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

routeDisruptionNameModifier :: String -> String
routeDisruptionNameModifier "_disruptionSummary" = "summary"
routeDisruptionNameModifier "_disruptionLevel" = "level"
routeDisruptionNameModifier s = defaultModifier s

instance Aeson.FromJSON RouteDisruption where
  parseJSON = genericParseJSON routeDisruptionNameModifier

instance Aeson.ToJSON RouteDisruption where
  toEncoding = genericToEncoding routeDisruptionNameModifier
  toJSON = genericToJSON routeDisruptionNameModifier

$(L.makeFields ''RouteDisruption)

data RouteStatus = RouteStatus
  { _statusSummary :: T.Text
  , _description   :: T.Text
  , _statusLevel   :: Int
  , _disruptions   :: [RouteDisruption]
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

routeStatusNameModifier :: String -> String
routeStatusNameModifier "_statusSummary" = "summary"
routeStatusNameModifier "_statusLevel" = "level"
routeStatusNameModifier s = defaultModifier s

instance Aeson.FromJSON RouteStatus where
  parseJSON = genericParseJSON routeStatusNameModifier

instance Aeson.ToJSON RouteStatus where
  toEncoding = genericToEncoding routeStatusNameModifier
  toJSON = genericToJSON routeStatusNameModifier

$(L.makeLenses ''RouteStatus)

data Route = Route
  { _routeName :: T.Text
  , _status    :: RouteStatus
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

routeNameModifier :: String -> String
routeNameModifier "_routeName" = "name"
routeNameModifier s = defaultModifier s

instance Aeson.FromJSON Route where
  parseJSON = genericParseJSON routeNameModifier

instance Aeson.ToJSON Route where
  toEncoding = genericToEncoding routeNameModifier

$(L.makeLenses ''Route)

data Grouping = Grouping
  { _groupingName :: T.Text
  , _groupingId   :: T.Text
  , _routes       :: Maybe [Route]
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

groupingNameModifier :: String -> String
groupingNameModifier "_groupingId" = "id"
groupingNameModifier "_groupingName" = "name"
groupingNameModifier s = defaultModifier s

instance Aeson.FromJSON Grouping where
  parseJSON = genericParseJSON groupingNameModifier

instance Aeson.ToJSON Grouping where
  toEncoding = genericToEncoding groupingNameModifier
  toJSON = genericToJSON groupingNameModifier

$(L.makeLenses ''Grouping)

data RouteStatusResponse = RouteStatusResponse
  { _lastUpdatedTime :: T.Text
  , _groupings       :: [Grouping]
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

instance Aeson.FromJSON RouteStatusResponse where
  parseJSON = genericParseJSON defaultModifier

instance Aeson.ToJSON RouteStatusResponse where
  toEncoding = genericToEncoding defaultModifier
  toJSON = genericToJSON defaultModifier

$(L.makeLenses ''RouteStatusResponse)
