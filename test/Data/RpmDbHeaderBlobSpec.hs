module Data.RpmDbHeaderBlobSpec (spec) where

import Data.ByteString.Lazy qualified as BLS
import Data.List (isSuffixOf)
import Data.Rpm.DbHeaderBlob (BlobInitErr (..), HeaderBlob (..), headerBlobInit)
import Test.Hspec (
  Spec,
  context,
  describe,
  fcontext,
  it,
  runIO,
  shouldBe,
  shouldSatisfy,
 )
import Debug.Trace (traceShowM)

spec :: Spec
spec = fcontext "" $ do
  testBlob <- runIO $ BLS.readFile "test/Data/test_data/pkg_blob.bin"
  headerBlobErrSpec
  headerBlobSpec testBlob

headerBlobSpec :: BLS.ByteString -> Spec
headerBlobSpec bs = describe "header blob parsing" $ do
  context "minimal example" $ do
    it "Parses data length and index length" $ do
      let testBS =
            BLS.pack
              [ 0
              , 0
              , 0
              , 1 -- indexLen
              , 0
              , 0
              , 0
              , 2 -- dataLen
              ]
      let fromBigEndianBlob =
            HeaderBlob
              { indexLength = 0x00000001
              , dataLength = 0x00000002
              , dataStart = 0x140 -- 0x40 + 0x01 * entryInfoSize
              , pvLength = 0xc2
              }
      headerBlobInit testBS `shouldBe` Right fromBigEndianBlob

  context "Real example " $ do
    it "Parses data length and index length" $ do
      let expected =
            HeaderBlob
              { indexLength = 0x00000050
              , dataLength = 0x00008ebc
              , dataStart = 0x475e40
              , pvLength = 0xb6fc -- 0x40 + 0x8ebc + 0x50 * entryInfoSize
              }
      headerBlobInit bs `shouldBe` Right expected

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
      let invalidDl = BLS.pack [0, 0x20, 0, 0 -- il
                               , 0, 0, 0, 1 -- dl
                               ]
      let res = headerBlobInit invalidDl 
      traceShowM res
      headerBlobInit invalidDl `shouldSatisfy` checkErr ("", 8) "blob size bad"
