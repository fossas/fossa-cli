module Data.RpmDbHeaderBlobSpec (spec) where

import Data.ByteString.Lazy qualified as BLS
import Data.List (isSuffixOf)
import Data.Rpm.DbHeaderBlob (headerBlobInit, HeaderBlob (..))
import Test.Hspec (
  Spec,
  describe,
  fcontext,
  it,
  runIO,
  shouldSatisfy, context, shouldBe,
 )

spec :: Spec
spec = fcontext "" $ do
  testBlob <- runIO $ BLS.readFile "test/Data/test_data/pkg_blob.bin"
  headerBlobErrSpec
  headerBlobSpec testBlob

headerBlobSpec :: BLS.ByteString -> Spec
headerBlobSpec bs = describe "header blob parsing" $ do
  context "minimal example" $ do
    it "Parses data length and index length" $ do
      let testBS = BLS.pack [0,0,0,1 -- indexLen
                            ,0,0,0,2] -- dataLen
      let fromBigEndianBlob = HeaderBlob {
            indexLength  = 0x00000001
            , dataLength = 0x00000002
            , dataStart = 0x140 -- 0x40 + 0x01 * entryInfoSize
            , pvLength = 0xc2
            }
      headerBlobInit testBS `shouldBe` Right fromBigEndianBlob

  context "Real example " $ do
    it "Parses data length and index length" $ do
      let expected = HeaderBlob {
            indexLength  = 0x00000050
            , dataLength = 0x00008ebc
            , dataStart = 0x475e40
            , pvLength = 0xb6fc -- 0x40 + 0x8ebc + 0x50 * entryInfoSize
            }
      headerBlobInit bs `shouldBe` Right expected

headerBlobErrSpec :: Spec
headerBlobErrSpec =
  describe "headerBlob parsing errors" $ do
    it "Should report failure when parsing nonexistent index length" $
      headerBlobInit ""
        `shouldSatisfy` ( \case
                            Left ("", 0, err) -> "Read indexLength" `isSuffixOf` err
                            _ -> False
                        )

    let invalidDl = BLS.pack [0, 0, 0, 1]
    it "Should report failure when parsing nonexistent data length" $
      headerBlobInit invalidDl
        `shouldSatisfy` ( \case
                            Left ("", 4, err) -> "Read dataLength" `isSuffixOf` err
                            _ -> False
                        )
