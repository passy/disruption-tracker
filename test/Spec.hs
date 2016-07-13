{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}

import           BasicPrelude
import           System.Directory       (getCurrentDirectory)

import           Control.Lens.At        (at)
import           Control.Lens.Cons      (_head)
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as BS
import qualified Data.Map.Strict        as Map
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO

import           Control.Lens.Operators
import           Test.Hspec
import           Test.Hspec.Expectations.Lens (shouldView, through)

import qualified Citymapper.Types       as Lib
import qualified Fixtures



readFixture :: FilePath -> IO BS.ByteString
readFixture path = do
    dir <- getCurrentDirectory
    BS.fromStrict . TE.encodeUtf8 <$> TIO.readFile (dir </> "test" </> "fixtures" </> path)

main :: IO ()
main = hspec $ do
  describe "Disruption Tracker" $ do
    describe "JSON Parsing" $ do
      it "parses a response" $ do
        resp <- readFixture "routestatus.json"
        let res = either error id (Aeson.eitherDecode resp) :: Lib.RouteStatusResponse
        res `shouldView` Lib.JSONDateTime Fixtures.lastUpdatedTime `through` Lib.lastUpdatedTime

      it "serializes to JSON" $ do
        let d = Lib.RouteDisruption { Lib._disruptionSummary = "Something bad"
                                    , Lib._stops = Nothing
                                    , Lib._disruptionLevel = 3
                                    }
        let s = Lib.RouteStatus { Lib._statusSummary = "It's down."
                                , Lib._description   = "I mean, it's real bad."
                                , Lib._statusLevel   = 3
                                , Lib._disruptions   = [d]
                                }
        let str = Aeson.encode s
        str `shouldBe` "{\"summary\":\"It's down.\",\"description\":\"I mean, it's real bad.\",\"level\":3,\"disruptions\":[{\"summary\":\"Something bad\",\"stops\":null,\"level\":3}]}"
