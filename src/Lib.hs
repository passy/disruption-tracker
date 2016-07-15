{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Text as T

disruptionUrl :: T.Text
disruptionUrl = "https://citymapper.com/api/1/routestatus?weekend=0"
