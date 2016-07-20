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

disruptionsTable :: R.Table
disruptionsTable = R.table "disruptions"

messengerSubscriptionsTable :: R.Table
messengerSubscriptionsTable = R.table "messenger_subscriptions"

data Host = Host { hostname :: T.Text
                 , port     :: Integer
                 , password :: Maybe T.Text }

data DisruptionRow = DisruptionRow
  { name        :: T.Text
  , description :: T.Text
  , disruptions :: [Citymapper.RouteDisruption]
  } deriving (Show, Eq, Generics.Generic, Aeson.FromJSON, Aeson.ToJSON, R.FromDatum, R.ToDatum, R.Expr)

connect :: Host -> IO R.RethinkDBHandle
connect Host { .. } = R.connect (T.unpack hostname) port (T.unpack <$> password)

setup :: Host -> IO R.Datum
setup host = do
  h <- connect host
  R.run' h . R.tableCreate $ disruptionsTable { R.tablePrimaryKey = Just "name" }
  R.run' h . R.tableCreate $ messengerSubscriptionsTable { R.tablePrimaryKey = Just "recipient_id" }

writeDisruptions :: Host -> DisruptionRow -> IO R.WriteResponse
writeDisruptions host s = do
  h <- connect host
  R.run h $ R.ex (disruptionsTable # R.insert s) [ R.conflict R.Replace ]
