module Data.RpmDbHeaderBlobSpec (spec) where

import Data.ByteString.Lazy qualified as BLS
import Data.Either (fromRight)
import Data.Int (Int32)
import Data.List (isSuffixOf)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Rpm.DbHeaderBlob (EntryInfo (..), HeaderBlob (..), IndexEntry (..), RegionInfo (..), calcDataLength, emptyRegionInfo, hdrblobImport, hdrblobVerifyRegion, headerBlobInit, regionTagCount, regionTagType, rpmStringType, rpmTagHeaderImg, rpmStringArrayType, rpmI18NstringType, rpmInt64Type)
import Test.Hspec (
  Expectation,
  Spec,
  context,
  describe,
  expectationFailure,
  fcontext,
  fdescribe,
  it,
  runIO,
  shouldBe,
  shouldContain,
  shouldMatchList,
  shouldSatisfy,
 )

-- This blob was output from an rpm sqlite db. The parts of the format
-- that we are interested in are documented in src/Data/Rpm/DbHeaderBlob.hs.
-- It's easiest to follow along using a hex editor, such as hexl or hex-fiend.
-- This blob is a v4 header
testBlob :: FilePath
testBlob = "test/Data/test_data/pkg_blob.bin"

-- the first two index entries that should result from parsing testBlob
testBlobIndexEntries :: [IndexEntry]
testBlobIndexEntries =
  [ IndexEntry
      { info = EntryInfo{tag = 1000, tagType = 6, offset = 2, count = 1}
      , entryLength = 7
      , rdLen = 0
      , entryData = BLS.pack [108, 105, 98, 103, 99, 99, 0]
      }
  , IndexEntry
      { info = EntryInfo{tag = 1004, tagType = 9, offset = 23, count = 1}
      , entryLength = 38
      , rdLen = 0
      , entryData = BLS.pack [71, 67, 67, 32, 118, 101, 114, 115, 105, 111, 110, 32, 49, 49, 32, 115, 104, 97, 114, 101, 100, 32, 115, 117, 112, 112, 111, 114, 116, 32, 108, 105, 98, 114, 97, 114, 121, 0]
      }
  ]

spec :: Spec
spec = fcontext "" $ do
  testBlob' <- runIO $ BLS.readFile testBlob
  headerBlobErrSpec
  headerBlobSpec testBlob'
  headerBlobVerifyRegionSpec testBlob'
  headerBlobImportSpec testBlob'
  dataLengthSpec

dataLengthSpec :: Spec
dataLengthSpec =
  fdescribe "dataLength" $ do
  context "rpm string type" $ do
    it "Fails for strings with too large of a count" $
      calcDataLength "" rpmStringType 2 0 0 `shouldBe` Left "count for string == 2 it should == 1."
    it "Fails if start > dataEnd" $
      calcDataLength "" rpmStringType 1 1 0 `shouldBe` Left "String start (1) >= end (0)"
    it "Calculates string lengths from the beginning of the buffer" $
        calcDataLength strData rpmStringType 1 0 7 `shouldBe` Right 3
    it "Calculates string lengths when start > 0" $
      calcDataLength strData rpmStringType 1 3 7`shouldBe` Right 4
  context "rpm string array type" $
    it "calculates length when count > 1" $
    calcDataLength strData rpmStringArrayType 2 0 7 `shouldBe` Right 7
  context "rpm i18nstring type" $
    it "calculates length when count > 1" $
    calcDataLength strData rpmI18NstringType 2 0 7 `shouldBe` Right 7
  context "rpm other types" $ do
    it "Fails on nonexistent type" $
      calcDataLength "" 255 0 0 0 `shouldBe` Left "Nonexistent typesize: 255"
    it "calculates length of an int64" $
      calcDataLength "" rpmInt64Type 2 0 0 `shouldBe` Right 16
    it "calculates length of an int64" $
      calcDataLength "" rpmInt64Type 2 0 0 `shouldBe` Right 16
  where
    strData =
      BLS.pack
        [99
        , 67
        , 0 
        , 99
        , 103
        , 104
        , 0
        , 255 -- this shouldn't matter, it's after a 0 and count should not be > 2
        ]

headerBlobImportSpec :: BLS.ByteString -> Spec
headerBlobImportSpec bs = do
  blob <- runIO . pure $ fromRight (error "RPM blob read failure") (headerBlobInit bs)
  describe "headerBlobImport" $ do
    it "Reads index entries from a non-dribble header blob" $ do
      hdrblobImport blob bs `shouldBe` Right testBlobIndexEntries

-- blobData should be read in from pkg_blob.bin.
headerBlobVerifyRegionSpec :: BLS.ByteString -> Spec
headerBlobVerifyRegionSpec blobData = do
  describe "headerBlobVerifyRegion" $ do
    it "Does nothing if not a header tag" $
      hdrblobVerifyRegion' notHeaderTag "unused" `shouldBe` (Right emptyRegionInfo)
    it "Fails on invalid region tag" $
      hdrblobVerifyRegion' invalidRegionTag "unused" `failsWithMsg` "invalid region tag"
    it "Fails on invalid region offset" $
      hdrblobVerifyRegion' invalidRegionOffset "unused" `failsWithMsg` "invalid region offset"
    it "Fails on trailer parse" $
      hdrblobVerifyRegion' goodInfo "unused" `failsWithMsg` "read trailer"
    it "Verifies a valid blob region" $
      hdrblobVerifyRegion' goodInfo blobData `shouldBe` (Right expectedRegionInfo)
  where
    failsWithMsg :: Either String RegionInfo -> String -> Expectation
    failsWithMsg e msg =
      case e of
        Right _ -> expectationFailure "Expected failure, got success"
        Left s -> s `shouldContain` msg

    expectedRegionInfo :: RegionInfo
    expectedRegionInfo =
      RegionInfo
        { rDl = 0x89b1
        , rIl = 0x45
        }

    testDataLength :: Int32
    testDataLength = 0x00008ebc

    testDataStart :: Int32
    testDataStart = 0x508

    hdrblobVerifyRegion' :: EntryInfo -> BLS.ByteString -> Either String RegionInfo
    hdrblobVerifyRegion' entryInfo = hdrblobVerifyRegion (NonEmpty.singleton entryInfo) testDataLength testDataStart

    goodInfo =
      EntryInfo
        { tag = 0x3f
        , tagType = 0x7
        , offset = 0x89a1
        , count = 0x10
        }

    baseInfo =
      EntryInfo
        { tag = 0
        , tagType = 0
        , offset = 0
        , count = 0
        }

    notHeaderTag =
      baseInfo
        { tag = -1
        , tagType = regionTagType
        , count = fromIntegral regionTagCount
        }

    invalidRegionTag =
      baseInfo
        { tag = rpmTagHeaderImg
        }

    invalidRegionOffset =
      goodInfo
        { offset = 0x8ebc
        }

emptyInfo :: NonEmpty.NonEmpty EntryInfo
emptyInfo = NonEmpty.singleton EntryInfo{tag = 0, tagType = 0, offset = 0, count = 0}

equalIgnoringEntries :: HeaderBlob -> HeaderBlob -> Expectation
equalIgnoringEntries b1 b2 =
  b1{entryInfos = emptyInfo} `shouldBe` b2{entryInfos = emptyInfo}

matchesIgnoringEntries :: (Show a) => Either a HeaderBlob -> HeaderBlob -> Expectation
matchesIgnoringEntries bs expected =
  case bs of
    Left b -> expectationFailure $ "Read header blob failed:" <> show b
    Right h -> h `equalIgnoringEntries` expected

headerBlobSpec :: BLS.ByteString -> Spec
headerBlobSpec bs = describe "header blob parsing" $ do
  context "Real example " $ do
    let eBlob = headerBlobInit bs
    blob <- runIO . pure $ fromRight (error "RPM blob read failure") eBlob

    it "Parses data length and index length" $ do
      let expected =
            HeaderBlob
              { indexLength = 0x00000050
              , dataLength = 0x00008ebc
              , dataStart = 0x508
              , dataEnd = 0x93c4
              , pvLength = 0x93c4
              , entryInfos = emptyInfo -- Not used in test
              , regionDataLength = 0x89b1 -- Not used in test
              , regionIndexLength = 0x45 -- Not used in test
              }
      eBlob `matchesIgnoringEntries` expected

    it "Parses entries" $ do
      -- this database is large, so we'll only check the first 2 entries
      let entries = take 2 . NonEmpty.toList . entryInfos $ blob

      entries
        `shouldMatchList` [ EntryInfo
                              { tag = 0x3f
                              , tagType = 0x7
                              , offset = 0x89a1
                              , count = 0x10
                              }
                          , EntryInfo
                              { tag = 0x64
                              , tagType = 0x8
                              , offset = 0
                              , count = 0x1
                              }
                          ]

headerBlobErrSpec :: Spec
headerBlobErrSpec =
  describe "headerBlob parsing errors" $ do
    let checkErr (size, offset) suffix res =
          case res of
            Left (size', offset', errStr) ->
              size' == size
                && offset' == offset
                && suffix `isSuffixOf` errStr
            _ -> False

    it "Should report failure when parsing nonexistent index length" $
      headerBlobInit ""
        `shouldSatisfy` checkErr ("", 0) "Read indexLength"

    it "Should report failure when parsing nonexistent data length" $ do
      let invalidDl = BLS.pack [0, 0, 0, 1]
      headerBlobInit invalidDl
        `shouldSatisfy` checkErr ("", 4) "Read dataLength"

    it "Should report too small index lengths" $ do
      let invalidIl = BLS.pack [0, 0, 0, 0]
      headerBlobInit invalidIl `shouldSatisfy` checkErr ("", 4) "region no tags error"

    it "Should report too small blob sizes" $ do
      let invalidDl =
            BLS.pack
              [ 1
              , 0
              , 0
              , 0 -- il
              , 0
              , 0
              , 0
              , 1 -- dl
              ]
      headerBlobInit invalidDl `shouldSatisfy` checkErr ("", 8) "blob size bad"
