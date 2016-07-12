{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Citymapper.Types   as Citymapper
import           Control.Monad      (void)

import qualified Database.RethinkDB as R

disruptionUrl :: String
disruptionUrl = "https://citymapper.com/api/1/routestatus?weekend=0"

disruptionsTable :: R.Table
disruptionsTable = R.table "disruptions"

writeRecord :: RouteStatus -> IO ()
writeRecord s = do
  h <- R.connect "192.168.99.100" 32772 Nothing
  void . R.run' h $ R.tableCreate disruptionsTable
  (void . R.run' h) $ R.insert s disruptionsTable
