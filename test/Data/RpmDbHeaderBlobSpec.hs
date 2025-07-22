module Data.RpmDbHeaderBlobSpec (spec) where

import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BLS
import Data.Int (Int32)
import Data.List (isSuffixOf)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Rpm.DbHeaderBlob.Internal (
  EntryMetadata (..),
  HeaderBlob (..),
  IndexCount,
  PkgInfo (..),
  RpmTag (..),
  RpmTagType (..),
  TagValueData (..),
  calcDataLength,
  getV3RegionCount,
  readHeaderBlobTagData,
  readHeaderMetaData,
  readPackageInfo,
 )
import Data.Set qualified as Set
import Test.Hspec (
  Expectation,
  Spec,
  context,
  describe,
  expectationFailure,
  it,
  runIO,
  shouldBe,
  shouldContain,
  shouldMatchList,
  shouldSatisfy,
 )

spec :: Spec
spec = context "RPM header blob parsing" $ do
  testBlob' <- runIO $ BLS.readFile testBlob
  headerBlobErrSpec
  headerBlobSpec testBlob'
  headerBlobV3RegionSpec testBlob'
  headerBlobImportSpec testBlob'
  dataLengthSpec
  readPackageSpec

-- This blob was output from an rpm sqlite db and is a v4 header.
-- The parts of the format that we are interested in are documented in src/Data/Rpm/DbHeaderBlob.hs.
testBlob :: FilePath
testBlob = "test/Data/test_data/pkg_blob.bin"

-- the first three non-dribble index entries that should result from parsing testBlob
testBlobTagValueData :: [TagValueData]
testBlobTagValueData =
  [ TagValueData
      { info = EntryMetadata{tag = TagHeaderI18nTable, tagType = RpmStringArray, offset = 0, count = 1}
      , entryLength = 2
      , entryData = BLS.pack [67, 0]
      }
  , TagValueData
      { info = EntryMetadata{tag = TagName, tagType = RpmString, offset = 2, count = 1}
      , entryLength = 7
      , entryData = BLS.pack [108, 105, 98, 103, 99, 99, 0]
      }
  , TagValueData
      { info = EntryMetadata{tag = TagVersion, tagType = RpmString, offset = 9, count = 1}
      , entryLength = 7
      , entryData = BLS.pack [49, 49, 46, 50, 46, 49, 0]
      }
  ]

readPackageSpec :: Spec
readPackageSpec = do
  -- These header blobs were all extracted from test dbs in go-rpmdb:
  -- https://github.com/knqyf263/go-rpmdb/tree/9f953f9/pkg/testdata
  ubi8Which <- runIO $ BLS.readFile "test/Data/test_data/ubi8-which2.21-17.el8-s390x.bin"
  centos5Vim <- runIO $ BLS.readFile "test/Data/test_data/centos5-vim-minimal-7.0.109-7.2.el5-x86_64.bin"
  sle15LibNCurses <- runIO $ BLS.readFile "test/Data/test_data/suse15-libncurses6-6.1-5.9.1-x86_64.bin"
  v3HeaderCentos6 <- runIO $ BLS.readFile "test/Data/test_data/centos6-devtools-gpg-pubkey-c105b9de-4e0fd3a3.bin"

  describe "read package data" $ do
    it "Reads big-endian bdb package: ubi8 Which" $
      readPackageInfo ubi8Which
        `shouldBe` Right
          PkgInfo
            { pkgName = Just "which"
            , pkgVersion = Just "2.21"
            , pkgRelease = Just "17.el8"
            , pkgArch = Just "s390x"
            , pkgEpoch = Nothing
            , pkgLicense = Nothing
            }

    it "Reads little-endian bdb package: centos5 Vim" $
      readPackageInfo centos5Vim
        `shouldBe` Right
          PkgInfo
            { pkgName = Just "vim-minimal"
            , pkgVersion = Just "7.0.109"
            , pkgRelease = Just "7.2.el5"
            , pkgArch = Just "x86_64"
            , pkgEpoch = Just 2
            , pkgLicense = Nothing
            }

    it "Reads ndb package: suse15 libncurses6" $
      readPackageInfo sle15LibNCurses
        `shouldBe` Right
          PkgInfo
            { pkgName = Just "libncurses6"
            , pkgVersion = Just "6.1"
            , pkgRelease = Just "5.9.1"
            , pkgArch = Just "x86_64"
            , pkgEpoch = Nothing
            , pkgLicense = Nothing
            }

  it "Reads package blob with a v3 header centos6-devtools gpg pubkey" $ do
    readPackageInfo v3HeaderCentos6
      `shouldBe` Right
        PkgInfo
          { pkgName = Just "gpg-pubkey"
          , pkgVersion = Just "c105b9de"
          , pkgRelease = Just "4e0fd3a3"
          , pkgArch = Nothing
          , pkgEpoch = Nothing
          }

dataLengthSpec :: Spec
dataLengthSpec =
  describe "dataLength" $ do
    context "rpm string type" $ do
      it "Fails for strings with too large of a count" $
        calcDataLength "" RpmString 2 0 0 `shouldBe` Left "count for string == 2 it should == 1."
      it "Fails if start > dataEnd" $
        calcDataLength "" RpmString 1 1 0 `shouldBe` Left "String start (1) >= end (0)"
      it "Calculates string lengths from the beginning of the buffer" $
        calcDataLength strData RpmString 1 0 7 `shouldBe` Right 3
      it "Calculates string lengths when start > 0" $
        calcDataLength strData RpmString 1 3 7 `shouldBe` Right 4
    context "rpm string array type" $
      it "calculates length when count > 1" $
        calcDataLength strData RpmStringArray 2 0 7 `shouldBe` Right 7
    context "rpm i18nstring type" $
      it "calculates length when count > 1" $
        calcDataLength strData RpmI18nString 2 0 7 `shouldBe` Right 7
    context "rpm other types" $ do
      it "calculates length of an int64" $
        calcDataLength "" RpmInt64 2 0 0 `shouldBe` Right 16
      it "calculates length of two int32" $
        calcDataLength "" RpmInt32 2 0 0 `shouldBe` Right 8
      it "calculates length of an int16" $
        calcDataLength "" RpmInt16 1 0 0 `shouldBe` Right 2
      it "calculates length of an int8" $
        calcDataLength "" RpmInt8 1 0 0 `shouldBe` Right 1
  where
    strData =
      BLS.pack
        [ 99
        , 67
        , 0
        , 99
        , 103
        , 104
        , 0
        , 255 -- this shouldn't matter, it's after a 0 and count should not be > 2
        ]

headerBlobInit' :: BLS.ByteString -> Either String HeaderBlob
headerBlobInit' = first (\(_, _, c) -> c) . readHeaderMetaData

headerBlobImportSpec :: BLS.ByteString -> Spec
headerBlobImportSpec bs = do
  describe "headerBlobImport" $ do
    it "Reads index entries from a non-dribble header blob" $ do
      let bl = headerBlobInit' bs >>= (readHeaderBlobTagData bs)
      case bl of
        Right importedIes -> testBlobTagValueData `shouldExistIn` importedIes
        Left e -> expectationFailure $ "Failed: " <> e
  where
    shouldExistIn :: (Ord a, Show a) => [a] -> [a] -> Expectation
    shouldExistIn a b =
      let a' = Set.fromList a
       in (a' `Set.intersection` Set.fromList b) `shouldBe` a'

headerBlobV3RegionSpec :: BLS.ByteString -> Spec
headerBlobV3RegionSpec blobData = do
  describe "headerBlobVerifyRegion" $ do
    it "Fails on invalid region tag" $
      getV3RegionCount' invalidRegionTag "unused" `failsWithMsg` "invalid region tag"
    it "Fails on invalid region offset" $
      getV3RegionCount' invalidRegionOffset "unused" `failsWithMsg` "invalid region offset"
    it "Fails on trailer parse" $
      getV3RegionCount' goodInfo "unused" `failsWithMsg` "read trailer"
    it "Verifies a valid blob region" $
      getV3RegionCount' goodInfo blobData `shouldBe` (Right expectedRegionInfo)
  where
    failsWithMsg :: Either String IndexCount -> String -> Expectation
    failsWithMsg e msg =
      case e of
        Right _ -> expectationFailure "Expected failure, got success"
        Left s -> s `shouldContain` msg

    expectedRegionInfo :: IndexCount
    expectedRegionInfo = 0x45

    testDataLength :: Int32
    testDataLength = 0x00008ebc

    testDataStart :: Int32
    testDataStart = 0x508

    getV3RegionCount' :: EntryMetadata -> BLS.ByteString -> Either String IndexCount
    getV3RegionCount' entryMeta = getV3RegionCount (NonEmpty.singleton entryMeta) testDataLength testDataStart

    goodInfo =
      EntryMetadata
        { tag = TagHeaderImmutable
        , tagType = RpmBin
        , offset = 0x89a1
        , count = 0x10
        }

    invalidRegionTag =
      EntryMetadata
        { tag = TagHeaderImage
        , tagType = RpmNull
        , offset = 0
        , count = 0
        }

    invalidRegionOffset =
      goodInfo
        { offset = 0x8ebc
        }

emptyInfo :: NonEmpty.NonEmpty EntryMetadata
emptyInfo =
  NonEmpty.singleton
    EntryMetadata
      { tag = TagName -- this is arbitrary, just needed a value
      , tagType = RpmNull
      , offset = 0
      , count = 0
      }

equalIgnoringEntries :: HeaderBlob -> HeaderBlob -> Expectation
equalIgnoringEntries b1 b2 =
  b1{entryMetadatas = emptyInfo} `shouldBe` b2{entryMetadatas = emptyInfo}

matchesIgnoringEntries :: (Show a) => Either a HeaderBlob -> HeaderBlob -> Expectation
matchesIgnoringEntries bs expected =
  case bs of
    Left b -> expectationFailure $ "Read header blob failed:" <> show b
    Right h -> h `equalIgnoringEntries` expected

headerBlobSpec :: BLS.ByteString -> Spec
headerBlobSpec bs = describe "header blob parsing" $ do
  context "Real example " $ do
    let eBlob = headerBlobInit' bs
    it "Parses data length and index length" $ do
      let expected =
            HeaderBlob
              { indexCount = 0x00000050
              , dataLength = 0x00008ebc
              , dataStart = 0x508
              , dataEnd = 0x93c4
              , entryMetadatas = emptyInfo -- Not used in test
              , regionIndexCount = 0x45 -- Not used in test
              }
      eBlob `matchesIgnoringEntries` expected

    it "Parses entries" $ do
      -- this database is large, so we'll only check the first 2 entries
      let entries = either (const []) (NonEmpty.take 2 . entryMetadatas) eBlob

      entries
        `shouldMatchList` [ EntryMetadata
                              { tag = TagHeaderImmutable
                              , tagType = RpmBin
                              , offset = 0x89a1
                              , count = 0x10
                              }
                          , EntryMetadata
                              { tag = TagHeaderI18nTable
                              , tagType = RpmStringArray
                              , offset = 0
                              , count = 0x1
                              }
                          ]

    it "Reads package info out of a test blob from sqlite" $
      readPackageInfo bs
        `shouldBe` Right
          PkgInfo
            { pkgName = Just "libgcc"
            , pkgVersion = Just "11.2.1"
            , pkgRelease = Just "1.fc35"
            , pkgArch = Just "x86_64"
            , pkgEpoch = Nothing
            }

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

    it "Should report failure when parsing nonexistent index count" $
      readHeaderMetaData ""
        `shouldSatisfy` checkErr ("", 0) "Read indexCount"

    it "Should report failure when parsing nonexistent data length" $ do
      let invalidDl = BLS.pack [0, 0, 0, 1]
      readHeaderMetaData invalidDl
        `shouldSatisfy` checkErr ("", 4) "Read dataLength"

    it "Should report too small index lengths" $ do
      let invalidIl = BLS.pack [0, 0, 0, 0]
      readHeaderMetaData invalidIl `shouldSatisfy` checkErr ("", 4) "region no tags error"

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
      readHeaderMetaData invalidDl `shouldSatisfy` checkErr ("", 8) "blob size bad"
