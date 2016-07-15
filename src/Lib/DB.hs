{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.DB where

import           Control.Monad        (void)
import qualified Data.Aeson           as Aeson
import qualified Data.Text            as T
import qualified Database.RethinkDB   as R
import qualified GHC.Generics         as Generics
import qualified Lib.Citymapper.Types as Citymapper

disruptionsTable :: R.Table
disruptionsTable = R.table "disruptions"

data DisruptionRow = DisruptionRow
  { name        :: T.Text
  , disruptions :: [Citymapper.RouteDisruption]
  } deriving (Show, Eq, Generics.Generic, Aeson.FromJSON, Aeson.ToJSON, R.FromDatum, R.ToDatum, R.Expr)

setup :: String -> Integer -> IO ()
setup hostname port = do
  h <- R.connect hostname port Nothing
  void . R.run' h $ R.tableCreate disruptionsTable

writeDisruptions :: DisruptionRow -> IO ()
writeDisruptions s = do
  h <- R.connect "192.168.99.100" 32772 Nothing
  void . R.run' h $ R.insert s disruptionsTable
