{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib.DB where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Database.RethinkDB as R
import qualified GHC.Generics as Generics
import qualified Lib.Citymapper.Types as C

import Database.RethinkDB (( # ))
import Control.Monad (void)
import Control.Lens ((^.), (^..))
import Data.Time.LocalTime (ZonedTime)

disruptionsTable :: R.Table
disruptionsTable = R.table "disruptions"

disruptionLogTable :: R.Table
disruptionLogTable = R.table "disruption_log"

routesInfoTable :: R.Table
routesInfoTable = R.table "routes_info"

messengerSubscriptionsTable :: R.Table
messengerSubscriptionsTable = R.table "messenger_subscriptions"

data Host = Host
  { hostname :: T.Text
  , port :: Integer
  , password :: Maybe T.Text
  }

data LinesRow = LinesRow
  { name :: T.Text
  , description :: T.Text
  , level :: Int
  , disruptions :: [C.RouteDisruption]
  } deriving (Show, Eq, Generics.Generic, Aeson.FromJSON, Aeson.ToJSON, R.FromDatum, R.ToDatum, R.Expr)

data LineLogRow = LineLogRow
  { name :: T.Text
  , description :: T.Text
  , level :: Int
  , disruptions :: [C.RouteDisruption]
  , timestamp :: ZonedTime
  } deriving (Show, Eq, Generics.Generic, Aeson.FromJSON, Aeson.ToJSON, R.FromDatum, R.ToDatum, R.Expr)

connect :: Host -> IO R.RethinkDBHandle
connect Host {..} = R.connect (T.unpack hostname) port (T.unpack <$> password)

routesAndColors :: [(T.Text, T.Text)]
routesAndColors =
  [ ("Bakerloo", "#894E24")
  , ("Central", "#DC241F")
  , ("Circle", "#FFCE00")
  , ("DLR", "#00AFAD")
  , ("District", "#007229")
  , ("Hammersmith & City", "#D799AF")
  , ("Jubilee", "#868F98")
  , ("Metropolitan", "#751056")
  , ("Northern", "#000000")
  , ("Overground", "#E86A10")
  , ("Piccadilly", "#0019A8")
  , ("TfL Rail", "#223589")
  , ("Victoria", "#00A0E2")
  , ("Waterloo & City", "#76D0BD")
  , ("Abellio Greater Anglia", "#003466")
  , ("Chiltern Railways", "#003466")
  , ("East Midlands Trains", "#003466")
  , ("Gatwick Express", "#E21020")
  , ("Grand Central", "#003466")
  , ("Great Northern", "#003466")
  , ("Great Western Railway", "#003466")
  , ("Heathrow Connect", "#003466")
  , ("Heathrow Express", "#532E63")
  , ("Hull Trains", "#003466")
  , ("London Midland", "#003466")
  , ("South West Trains", "#003466")
  , ("Southeastern", "#003466")
  , ("Southern", "#003466")
  , ("Thameslink", "#003466")
  , ("Virgin Trains", "#003466")
  , ("Virgin Trains East Coast", "#003466")
  , ("c2c", "#003466")
  , ("Tram", "#7EB200")
  , ("RB1X", "#00A0E2")
  , ("RB1", "#00A0E2")
  , ("RB2", "#00A0E2")
  , ("RB4", "#00A0E2")
  , ("RB5", "#00A0E2")
  , ("RB6", "#00A0E2")
  , ("Elizabeth", "#7156A5")
  ]

setup :: Host -> IO ()
setup host = do
  h <- connect host
  void . R.run' h $
    disruptionsTable
    { R.tablePrimaryKey = Just "name"
    } #
    R.tableCreate
  void . R.run' h $ disruptionLogTable # R.tableCreate
  void . R.run' h $ disruptionLogTable # R.indexCreate "name" (R.! "name")
  void . R.run' h $
    routesInfoTable
    { R.tablePrimaryKey = Just "name"
    } #
    R.tableCreate
  void . R.run' h $
    messengerSubscriptionsTable
    { R.tablePrimaryKey = Just "route"
    } #
    R.tableCreate
  void . R.run' h $ messengerSubscriptionsTable #
    R.indexCreate "recipients" (R.! "recipients")
  writeRoutes host routesAndColors

writeRoutes :: Host -> [(T.Text, T.Text)] -> IO ()
writeRoutes host routes = do
  h <- connect host
  void . R.run' h $ routesInfoTable # R.delete
  void . R.run' h $ routesInfoTable #
    R.insert
      (map
         (\(n, color) ->
             ["name" R.:= T.toLower n, "display" R.:= n, "color" R.:= color])
         routes)

writeDisruptions :: Host -> C.Route -> IO ()
writeDisruptions host route = do
  h <- connect host
  void $
    R.run'
      h
      [ R.ex (disruptionsTable # R.insert (toLine route)) [R.conflict R.Replace]
      , disruptionLogTable # R.insert (toLog route)
      ]
  where
    toLine r =
      LinesRow
        (r ^. C.routeName)
        (r ^. C.status . C.description)
        (r ^. C.status . C.statusLevel)
        (r ^.. C.status . C.disruptions . traverse)
    toLog r =
      LineLogRow
        (r ^. C.routeName)
        (r ^. C.status . C.description)
        (r ^. C.status . C.statusLevel)
        (r ^.. C.status . C.disruptions . traverse)
