{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Protolude hiding (to)

import Test.Hspec
import System.Directory (getCurrentDirectory)
import Test.Hspec.Expectations.Lens (shouldView, through)
import Control.Lens (to)
import System.FilePath ((</>))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

import qualified Fixtures
import qualified Lib.Citymapper.Types as Lib

readFixture :: FilePath -> IO BS.ByteString
readFixture path = do
  dir <- getCurrentDirectory
  BS.fromStrict . TE.encodeUtf8 <$>
    readFile (dir </> "test" </> "fixtures" </> path)

main :: IO ()
main =
  hspec . describe "Disruption Tracker" . describe "JSON Parsing" . parallel $
  do it "parses a response" $
       do resp <- readFixture "routestatus.json"
          let res =
                either error identity (first T.pack $ Aeson.eitherDecode resp) :: Lib.RouteStatusResponse
          res `shouldView` Lib.JSONDateTime Fixtures.lastUpdatedTime `through`
            Lib.lastUpdatedTime
     it "parses a response with a different status" $
       do resp <- readFixture "routestatus_1.json"
          let res =
                either error identity (first T.pack $ Aeson.eitherDecode resp) :: Lib.RouteStatusResponse
          res `shouldView` 8 `through` Lib.groupings . to length
     it "serializes to JSON" $
       do let d =
                Lib.RouteDisruption
                { Lib._disruptionSummary = "Something bad"
                , Lib._stops = Nothing
                , Lib._disruptionLevel = Lib.SevereDelays
                }
          let s =
                Lib.RouteStatus
                { Lib._statusSummary = "It's down."
                , Lib._description = "I mean, it's real bad."
                , Lib._statusLevel = Lib.PartSuspended
                , Lib._disruptions = [d]
                }
          let str = Aeson.encode s
          str `shouldBe`
            "{\"summary\":\"It's down.\",\"description\":\"I mean, it's real bad.\",\"level\":3,\"disruptions\":[{\"summary\":\"Something bad\",\"stops\":null,\"level\":2}]}"
