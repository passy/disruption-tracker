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

linesTable :: R.Table
linesTable = R.table "lines"

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

setup :: Host -> IO ()
setup host = do
  h <- connect host
  void . R.run' h $ linesTable { R.tablePrimaryKey = Just "name" } # R.tableCreate
  void . R.run' h $ messengerSubscriptionsTable # R.tableCreate
  void . R.run' h $ linesTable # R.indexCreate "line" (R.! "line")

writeDisruptions :: Host -> LinesRow -> IO R.WriteResponse
writeDisruptions host s = do
  h <- connect host
  R.run h $ R.ex (linesTable # R.insert s) [ R.conflict R.Replace ]
