{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Citymapper.Types where

import Protolude

import qualified Control.Lens.TH as L
import qualified Data.Aeson.Casing as AesonC
import qualified Data.Aeson.Types as Aeson
import qualified Data.Hourglass as Hourglass
import qualified Data.Hourglass.Compat as HourglassC
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Database.RethinkDB as R
import qualified GHC.Generics as Generics

import Control.Applicative (empty, (<*>))
import Control.Monad (MonadPlus(), mzero, fail)
import Data.Aeson ((.:), (.:?), (.!=))
import Data.String (String)

maybeParse
  :: MonadPlus m
  => (a -> Maybe b) -> m a -> m b
maybeParse f = (maybe mzero return . f =<<)

fromDateTimeStr
  :: MonadPlus m
  => m String -> m Hourglass.DateTime
fromDateTimeStr = maybeParse (Hourglass.timeParse Hourglass.ISO8601_DateAndTime)

defaultModifier :: String -> String
defaultModifier = AesonC.snakeCase . drop 1

genericParseJSON
  :: (Generics.Generic a, Aeson.GFromJSON (Generics.Rep a))
  => (String -> String) -> Aeson.Value -> Aeson.Parser a
genericParseJSON modifier =
  Aeson.genericParseJSON
    Aeson.defaultOptions
    { Aeson.fieldLabelModifier = modifier
    }

genericToEncoding
  :: (Generics.Generic a, Aeson.GToEncoding (Generics.Rep a))
  => (String -> String) -> a -> Aeson.Encoding
genericToEncoding modifier =
  Aeson.genericToEncoding
    Aeson.defaultOptions
    { Aeson.fieldLabelModifier = modifier
    }

genericToJSON
  :: (Generics.Generic a, Aeson.GToJSON (Generics.Rep a))
  => (String -> String) -> a -> Aeson.Value
genericToJSON modifier =
  Aeson.genericToJSON
    Aeson.defaultOptions
    { Aeson.fieldLabelModifier = modifier
    }

data DisruptionLevel
  = GoodService
  | MinorDelays
  | SevereDelays
  | PartSuspended
  | UnknownLevel Int
  deriving (Show, Eq, Ord)

instance R.ToDatum DisruptionLevel where
  toDatum = R.Number . fromIntegral . disruptionLevelToInt

instance R.FromDatum DisruptionLevel where
  parseDatum (R.Number n) = pure . intToDisruptionLevel $ floor n
  parseDatum _ = fail "Unsupport Datum type"

instance R.Expr DisruptionLevel

instance Aeson.FromJSON DisruptionLevel where
  parseJSON (Aeson.Number i) = pure . intToDisruptionLevel $ floor i
  parseJSON _ = fail "Unsupported disruption level JSON type"

instance Aeson.ToJSON DisruptionLevel where
  toJSON = Aeson.Number . fromIntegral . disruptionLevelToInt

intToDisruptionLevel :: Int -> DisruptionLevel
intToDisruptionLevel = \case
  0 -> GoodService
  1 -> MinorDelays
  2 -> SevereDelays
  3 -> PartSuspended
  i -> UnknownLevel i

disruptionLevelToInt :: DisruptionLevel -> Int
disruptionLevelToInt = \case
  GoodService -> 0
  MinorDelays -> 1
  SevereDelays -> 2
  PartSuspended -> 3
  UnknownLevel i -> i

showDisruptionLevel :: DisruptionLevel -> String
showDisruptionLevel = \case
  GoodService -> "Good Service"
  MinorDelays -> "Minor Delays"
  SevereDelays -> "Severe Delays"
  PartSuspended -> "Part Suspended"
  UnknownLevel i -> "Unknown Status " <> show i

newtype JSONDateTime =
  JSONDateTime Hourglass.DateTime
  deriving (Show, Eq)

instance Aeson.ToJSON JSONDateTime where
  toJSON (JSONDateTime d) =
    Aeson.String <$> T.pack $
    Hourglass.timePrint Hourglass.ISO8601_DateAndTime d

instance Aeson.FromJSON JSONDateTime where
  parseJSON (Aeson.String s) =
    JSONDateTime <$> fromDateTimeStr (pure $ T.unpack s)
  parseJSON _ = fail "Invalid JSONDateTime"

timeTranspose :: Time.ZonedTime
          -> Hourglass.LocalTime Hourglass.DateTime
timeTranspose oldTime =
    Hourglass.localTime
        offsetTime
        (Hourglass.DateTime newDate timeofday)
  where
    newDate :: Hourglass.Date
    newDate = HourglassC.dateFromTAIEpoch $ Time.toModifiedJulianDay $ Time.localDay $ Time.zonedTimeToLocalTime oldTime

    timeofday :: Hourglass.TimeOfDay
    timeofday = HourglassC.diffTimeToTimeOfDay $ Time.timeOfDayToTime $ Time.localTimeOfDay $ Time.zonedTimeToLocalTime oldTime

    offsetTime = Hourglass.TimezoneOffset $ fromIntegral $ Time.timeZoneMinutes $ Time.zonedTimeZone oldTime

instance R.ToDatum JSONDateTime where
  toDatum (JSONDateTime h) = R.Time $ Time.ZonedTime localTime Time.utc
    where
      hdate :: Hourglass.Date
      hdate = Hourglass.timeGetDate h
      day :: Time.Day
      day =
        Time.fromGregorian
          (fromIntegral $ Hourglass.dateYear hdate)
          (fromEnum $ Hourglass.dateMonth hdate)
          (Hourglass.dateDay hdate)
      htime :: Hourglass.TimeOfDay
      htime = Hourglass.timeGetTimeOfDay h
      ttime :: Time.TimeOfDay
      ttime =
        Time.TimeOfDay
          (fromEnum $ Hourglass.todHour htime)
          (fromEnum $ Hourglass.todMin htime)
          (fromIntegral $ fromEnum $ Hourglass.todSec htime)
      localTime :: Time.LocalTime
      localTime = Time.LocalTime day ttime

instance R.FromDatum JSONDateTime where
  parseDatum (R.Time z) = return . JSONDateTime $ Hourglass.localTimeUnwrap $ timeTranspose z
  parseDatum _ = fail "Unsupported datum"

instance R.Expr JSONDateTime

data RouteDisruption = RouteDisruption
  { _disruptionSummary :: T.Text
  , _stops :: Maybe [T.Text]
  , _disruptionLevel :: DisruptionLevel
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
  , _description :: T.Text
  , _statusLevel :: DisruptionLevel
  , _disruptions :: [RouteDisruption]
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

defRouteStatus :: RouteStatus
defRouteStatus =
  RouteStatus
  { _statusSummary = "Unknown Status"
  , _description = "The status of this line is currently unknown."
  , _statusLevel = UnknownLevel 0
  , _disruptions = []
  }

routeStatusNameModifier :: String -> String
routeStatusNameModifier "_statusSummary" = "summary"
routeStatusNameModifier "_statusLevel" = "level"
routeStatusNameModifier s = defaultModifier s

instance Aeson.FromJSON RouteStatus where
  parseJSON =
    Aeson.withObject "status" $
    \o ->
       RouteStatus <$> o .: "summary" <*> o .: "description" .!= mempty <*>
       o .: "level" <*>
       o .: "disruptions"

instance Aeson.ToJSON RouteStatus where
  toEncoding = genericToEncoding routeStatusNameModifier
  toJSON = genericToJSON routeStatusNameModifier

$(L.makeLenses ''RouteStatus)

data Route = Route
  { _routeName :: T.Text
  , _status :: RouteStatus
  } deriving (Show, Eq, Generics.Generic, R.FromDatum, R.ToDatum, R.Expr)

routeNameModifier :: String -> String
routeNameModifier "_routeName" = "name"
routeNameModifier s = defaultModifier s

instance Aeson.FromJSON Route where
  parseJSON =
    Aeson.withObject "route" $
    \o -> Route <$> o .: "name" <*> o .:? "status" .!= defRouteStatus

instance Aeson.ToJSON Route where
  toEncoding = genericToEncoding routeNameModifier

$(L.makeLenses ''Route)

data Grouping = Grouping
  { _groupingName :: T.Text
  , _groupingId :: T.Text
  , _routes :: Maybe [Route]
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
  { _lastUpdatedTime :: JSONDateTime
  , _groupings :: [Grouping]
  } deriving (Show, Eq, Generics.Generic)

instance Aeson.FromJSON RouteStatusResponse where
  parseJSON (Aeson.Object v) =
    RouteStatusResponse <$> v .: "last_updated_time" <*> v .: "groupings"
  parseJSON _ = empty

instance Aeson.ToJSON RouteStatusResponse where
  toEncoding = genericToEncoding defaultModifier
  toJSON = genericToJSON defaultModifier

$(L.makeLenses ''RouteStatusResponse)
