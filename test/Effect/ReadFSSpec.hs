{-# LANGUAGE TemplateHaskell #-}

module Effect.ReadFSSpec (
  spec,
) where

import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effect.ReadFS (readContentsBS, readContentsJson)
import Path (mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe, runIO)

expectedContents :: Map Text Text
expectedContents = Map.fromList [("name", "example"), ("version", "1.0.0")]

spec :: Spec
spec = do
  currentDir <- runIO getCurrentDir
  let testdata = currentDir </> $(mkRelFile "test/Effect/testdata/no-bom.json")
  let testdataBom = currentDir </> $(mkRelFile "test/Effect/testdata/utf8-bom.json")

  describe "readContentsJson" $ do
    it' "parses a JSON file" $ do
      contents <- readContentsJson testdata
      contents `shouldBe'` expectedContents

    it' "parses a JSON file with a leading UTF-8 byte order mark" $ do
      contents <- readContentsJson testdataBom
      contents `shouldBe'` expectedContents

    -- The fixture must literally begin with EF BB BF. Those bytes are invisible
    -- in a diff, so this assertion is the only thing standing between us and an
    -- editor silently "cleaning up" the file.
    it' "fixture actually begins with a UTF-8 BOM" $ do
      raw <- readContentsBS testdataBom
      BS.take 3 raw `shouldBe'` "\xEF\xBB\xBF"
