module Data.RpmDbHeaderBlobSpec (spec) where

import Data.ByteString.Lazy qualified as BLS
import Data.Either (fromRight)
import Data.List (isSuffixOf)
import Data.Rpm.DbHeaderBlob (EntryInfo (..), HeaderBlob (..), headerBlobInit)
import Test.Hspec (
  Expectation,
  Spec,
  context,
  describe,
  expectationFailure,
  fcontext,
  it,
  runIO,
  shouldMatchList,
  shouldSatisfy,
 )

spec :: Spec
spec = fcontext "" $ do
  testBlob <- runIO $ BLS.readFile "test/Data/test_data/pkg_blob.bin"
  headerBlobErrSpec
  headerBlobSpec testBlob

equalIgnoringEntries :: HeaderBlob -> HeaderBlob -> Bool
equalIgnoringEntries b1 b2 =
  b1{entryInfos = []} == b2{entryInfos = []}

matchesIgnoringEntries :: Either a HeaderBlob -> HeaderBlob -> Expectation
matchesIgnoringEntries bs expected =
  case bs of
    Left _ -> expectationFailure "Read header blob"
    Right h -> h `shouldSatisfy` equalIgnoringEntries expected

headerBlobSpec :: BLS.ByteString -> Spec
headerBlobSpec bs = describe "header blob parsing" $ do
  -- I'm not convinced this is valuable. It will take some effort to keep
  -- updating this test/binary as we read.
  -- context "minimal example" $ do
  --   it "Parses data length and index length" $ do
  --     let testBs =
  --           BLS.pack
  --             [ --indexLen
  --               0
  --             , 0
  --             , 0
  --             , 1
  --             -- end indexLen
  --             -- dataLen
  --             , 0
  --             , 0
  --             , 0
  --             , 2
  --             -- end dataLen
  --             ]
  --     let fromBigEndianBlob =
  --           HeaderBlob
  --             { indexLength = 0x00000001
  --             , dataLength = 0x00000002
  --             , dataStart = 0x140 -- 0x40 + 0x01 * entryInfoSize
  --             , pvLength = 0xc2
  --             , entryInfos = []
  --             }

  --     (headerBlobInit testBs) `matchesIgnoringEntries` fromBigEndianBlob

  context "Real example " $ do
    let eBlob = headerBlobInit bs
    blob <- runIO . pure $ fromRight (error "RPM blob read failure") eBlob

    it "Parses data length and index length" $ do
      let expected =
            HeaderBlob
              { indexLength = 0x00000050
              , dataLength = 0x00008ebc
              , dataStart = 0x475e40
              , pvLength = 0xb6fc -- 0x40 + 0x8ebc + 0x50 * entryInfoSize
              , entryInfos = []
              }
      eBlob `matchesIgnoringEntries` expected

    it "Parses entries" $ do
      -- this database is large, so we'll only check the first 2 entries
      let entries = take 2 $ entryInfos blob

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

    it "Should report too large blob sizes" $ do
      let invalidDl =
            BLS.pack
              [ 0
              , 0x20
              , 0
              , 0 -- il
              , 0
              , 0
              , 0
              , 1 -- dl
              ]
      headerBlobInit invalidDl `shouldSatisfy` checkErr ("", 8) "blob size bad"
