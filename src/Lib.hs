{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib where

import qualified Data.Text          as T
import qualified Data.Text.Format   as TF
import qualified Data.Text.Lazy.IO       as TLIO
import qualified Data.Text.Lazy     as TL
import qualified Database.RethinkDB as R

import           Control.Monad      (guard)

disruptionUrl :: T.Text
disruptionUrl = "https://citymapper.com/api/1/routestatus?weekend=0"

summarizeWriteResponse :: [R.WriteResponse] -> TL.Text
summarizeWriteResponse =
  let tplus (a, b) (a', b') = (a + a', b + b')
      extract R.WriteResponse {..} = (writeResponseInserted, writeResponseReplaced)
      format :: (Int, Int) -> TL.Text
      format = TF.format "Inserted: {}, updated: {}"
  in format . foldl (\b a -> b `tplus` extract a) (0, 0)

printIfNonEmpty :: TL.Text -> IO ()
printIfNonEmpty t = do
  guard . not . TL.null $ t
  TLIO.putStrLn t
