{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib where

import qualified Data.Text          as T
import qualified Data.Text.Format   as TF
import qualified Data.Text.Lazy     as TL
import qualified Database.RethinkDB as R

disruptionUrl :: T.Text
disruptionUrl = "https://citymapper.com/api/1/routestatus?weekend=0"

summarizeWriteResponse :: [R.WriteResponse] -> Maybe TL.Text
summarizeWriteResponse =
  let tplus (a, b) (a', b') = (a + a', b + b')
      extract R.WriteResponse {..} = (writeResponseInserted, writeResponseReplaced)
      format :: (Int, Int) -> Maybe TL.Text
      format (0, 0) = Nothing
      format a      = Just $ TF.format "Inserted: {}, updated: {}" a
  in format . foldl (\b a -> b `tplus` extract a) (0, 0)
