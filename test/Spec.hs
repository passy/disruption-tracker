{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}

import           BasicPrelude
import           Data.Maybe             (fromJust)
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

import qualified Lib                    as Lib

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
        print (res :: Lib.RouteStatusResponse)
