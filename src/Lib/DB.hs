{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib.DB where

import qualified Data.Aeson           as Aeson
import qualified Data.Text            as T
import qualified Database.RethinkDB   as R
import qualified GHC.Generics         as Generics
import qualified Lib.Citymapper.Types as Citymapper

import Database.RethinkDB ((#))
import Control.Monad (void)

disruptionsTable :: R.Table
disruptionsTable = R.table "disruptions"

routesInfoTable :: R.Table
routesInfoTable = R.table "routes_info"

messengerSubscriptionsTable :: R.Table
messengerSubscriptionsTable = R.table "messenger_subscriptions"

data Host = Host { hostname :: T.Text
                 , port     :: Integer
                 , password :: Maybe T.Text }

data LinesRow = LinesRow
  { name        :: T.Text
  , description :: T.Text
  , level       :: Int
  , disruptions :: [Citymapper.RouteDisruption]
  } deriving (Show, Eq, Generics.Generic, Aeson.FromJSON, Aeson.ToJSON, R.FromDatum, R.ToDatum, R.Expr)

connect :: Host -> IO R.RethinkDBHandle
connect Host { .. } = R.connect (T.unpack hostname) port (T.unpack <$> password)

supportedRoutes :: [(T.Text, T.Text)]
supportedRoutes =
  [ ("Bakerloo", "bakerloo.svg")
  , ("Central", "central.svg")
  , ("Circle", "circle.svg")
  , ("DLR", "dlr.svg")
  , ("District", "district.svg")
  , ("Hammersmith & City", "hac.svg")
  , ("Jubilee", "jubilee.svg")
  , ("Metropolitan", "metropolitan.svg")
  , ("Northern", "northern.svg")
  , ("Overground", "overground.svg")
  , ("Piccadilly", "piccadilly.svg")
  , ("TfL Rail", "rail.svg")
  , ("Victoria", "victoria.svg")
  , ("Waterloo & City", "wac.svg")
  , ("Abellio Greater Anglia", "")
  , ("Chiltern Railways", "")
  , ("East Midlands Trains", "")
  , ("Gatwick Express", "")
  , ("Grand Central", "")
  , ("Great Northern", "")
  , ("Great Western Railway", "")
  , ("Heathrow Connect", "")
  , ("Heathrow Express", "")
  , ("Hull Trains", "")
  , ("London Midland", "")
  , ("South West Trains", "")
  , ("Southeastern", "")
  , ("Southern", "")
  , ("Thameslink", "")
  , ("Virgin Trains", "")
  , ("Virgin Trains East Coast", "")
  , ("c2c", "")
  , ("Tram", "tramlink.svg")
  , ("RB1X", "")
  , ("RB1", "")
  , ("RB2", "")
  , ("RB4", "")
  , ("RB5", "")
  , ("RB6", "")
  , ("Elizabeth", "")
  ]

setup :: Host -> IO ()
setup host = do
  h <- connect host
  void . R.run' h $ disruptionsTable { R.tablePrimaryKey = Just "name" } # R.tableCreate
  void . R.run' h $ routesInfoTable { R.tablePrimaryKey = Just "name" } # R.tableCreate
  void . R.run' h $ messengerSubscriptionsTable { R.tablePrimaryKey = Just "route" } # R.tableCreate
  void . R.run' h $ messengerSubscriptionsTable # R.indexCreate "recipients" (R.! "recipients")
  writeRoutes host supportedRoutes

writeRoutes :: Host -> [(T.Text, T.Text)] -> IO ()
writeRoutes host routes = do
  h <- connect host
  void . R.run' h $ routesInfoTable # R.delete
  void . R.run' h $ routesInfoTable #
    R.insert (map (\(n, url) -> ["name" R.:= n, "image_name" R.:= url]) routes)

writeDisruptions :: Host -> LinesRow -> IO R.WriteResponse
writeDisruptions host s = do
  h <- connect host
  R.run h $ R.ex (disruptionsTable # R.insert s) [ R.conflict R.Replace ]
