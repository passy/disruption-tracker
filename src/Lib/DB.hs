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

disruptionsTable :: R.Table
disruptionsTable = R.table "disruptions"

data Host = Host { hostname :: T.Text
                 , port     :: Integer
                 , password :: Maybe T.Text }

data DisruptionRow = DisruptionRow
  { name        :: T.Text
  , disruptions :: [Citymapper.RouteDisruption]
  } deriving (Show, Eq, Generics.Generic, Aeson.FromJSON, Aeson.ToJSON, R.FromDatum, R.ToDatum, R.Expr)

connect :: Host -> IO R.RethinkDBHandle
connect Host { .. } = R.connect (T.unpack hostname) port (T.unpack <$> password)

setup :: Host -> IO R.Datum
setup host = do
  h <- connect host
  R.run' h . R.tableCreate $ disruptionsTable { R.tablePrimaryKey = Just "name" }

writeDisruptions :: Host -> DisruptionRow -> IO R.WriteResponse
writeDisruptions host s = do
  h <- connect host
  R.run h $ R.replace (const s) disruptionsTable
