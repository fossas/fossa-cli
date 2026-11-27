{-# LANGUAGE TemplateHaskell #-}

module Effect.ReadFSSpec (
  spec,
) where

import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effect.ReadFS (readContentsBS, readContentsJson, readContentsJsonc, readContentsText)
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
  let testdataUtf16LE = currentDir </> $(mkRelFile "test/Effect/testdata/utf16le-bom.json")
  let testdataUtf16BE = currentDir </> $(mkRelFile "test/Effect/testdata/utf16be-bom.json")

  describe "readContentsText" $ do
    -- Windows tooling sometimes saves manifests as UTF-16 with a byte order
    -- mark; read as UTF-8 they decode to NUL-riddled text no parser accepts.
    it' "decodes UTF-16 files by their byte order mark" $ do
      expected <- readContentsText testdata
      littleEndian <- readContentsText testdataUtf16LE
      bigEndian <- readContentsText testdataUtf16BE
      littleEndian `shouldBe'` expected
      bigEndian `shouldBe'` expected

    it' "fixtures actually begin with UTF-16 byte order marks" $ do
      littleEndian <- readContentsBS testdataUtf16LE
      bigEndian <- readContentsBS testdataUtf16BE
      BS.take 2 littleEndian `shouldBe'` "\xFF\xFE"
      BS.take 2 bigEndian `shouldBe'` "\xFE\xFF"

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

    it' "parses UTF-16 JSON files with a byte order mark" $ do
      littleEndian <- readContentsJson testdataUtf16LE
      bigEndian <- readContentsJson testdataUtf16BE
      littleEndian `shouldBe'` expectedContents
      bigEndian `shouldBe'` expectedContents

  describe "readContentsJsonc" $ do
    it' "parses a JSON file with a leading UTF-8 byte order mark" $ do
      contents <- readContentsJsonc testdataBom
      contents `shouldBe'` expectedContents

    it' "parses a UTF-16 JSON file with a byte order mark" $ do
      contents <- readContentsJsonc testdataUtf16LE
      contents `shouldBe'` expectedContents
