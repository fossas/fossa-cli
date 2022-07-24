module App.Fossa.VSI.FingerprintSpec (spec) where

import App.Fossa.VSI.Fingerprint (Combined (..), fingerprint)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Data.Aeson (encode, toJSON)
import Data.String.Conversion (toText)
import Effect.ReadFS (runReadFSIO)
import Path (Abs, File, Path)
import Path.IO qualified as PIO
import ResultUtil
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "derives ToJSON" $ do
    target <- runIO fileBinary
    result <- runIO . runStack . runDiagnostics . runReadFSIO $ fingerprint target

    it "derives ToJSON correctly" $
      assertOnSuccess result $ \_ c ->
        (encode . toJSON . combinedRaw $ c) `shouldBe` "\"a57d87677b2b67c573c870aff75d8c92c23beaf9f8682e7749ba7b421e84c991\""

  describe "content is binary" $ do
    target <- runIO fileBinary
    result <- runIO . runStack . runDiagnostics . runReadFSIO $ fingerprint target

    it "fingerprints raw correctly" $
      assertOnSuccess result $ \_ c ->
        (toText . combinedRaw $ c) `shouldBe` "a57d87677b2b67c573c870aff75d8c92c23beaf9f8682e7749ba7b421e84c991"

    it "fingerprints comment stripped correctly" $
      assertOnSuccess result $ \_ c ->
        case (combinedCommentStripped c) of
          Nothing -> pure ()
          Just _ -> expectationFailure "should not have comment strip fingerprinted binary file"

  describe "content is empty" $ do
    target <- runIO fileEmpty
    result <- runIO . runStack . runDiagnostics . runReadFSIO $ fingerprint target

    it "fingerprints raw correctly" $
      assertOnSuccess result $
        \_ c -> (toText . combinedRaw $ c) `shouldBe` "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

    it "fingerprints comment stripped correctly" $
      assertOnSuccess result $ \_ c ->
        case (combinedCommentStripped c) of
          Nothing -> expectationFailure "should have comment strip fingerprinted file"
          Just fp -> (toText fp) `shouldBe` "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

  describe "single line file" $ do
    target <- runIO fileSingleLine
    result <- runIO . runStack . runDiagnostics . runReadFSIO $ fingerprint target

    it "fingerprints raw correctly" $
      assertOnSuccess result $ \_ c ->
        (toText . combinedRaw $ c) `shouldBe` "deac66ccb79f6d31c0fa7d358de48e083c15c02ff50ec1ebd4b64314b9e6e196"

    it "fingerprints comment stripped correctly" $
      assertOnSuccess result $ \_ c ->
        case (combinedCommentStripped c) of
          Nothing -> expectationFailure "should have comment strip fingerprinted file"
          Just fp -> (toText fp) `shouldBe` "80a7161009ffaf868641acac3f5e49bc5f86021ee1d177f3b1cbb47573513649"

  describe "single line file with comment" $ do
    target <- runIO fileSingleLineComment
    result <- runIO . runStack . runDiagnostics . runReadFSIO $ fingerprint target

    it "fingerprints raw correctly" $
      assertOnSuccess result $ \_ c ->
        (toText . combinedRaw $ c) `shouldBe` "7bf434baac2af7cf06fea2125d453f7b14cf942d5b3cef2a4a2a9281eec2534b"

    it "fingerprints comment stripped correctly" $
      assertOnSuccess result $ \_ c ->
        case (combinedCommentStripped c) of
          Nothing -> expectationFailure "should have comment strip fingerprinted file"
          Just fp -> (toText fp) `shouldBe` "80a7161009ffaf868641acac3f5e49bc5f86021ee1d177f3b1cbb47573513649"

  describe "multi line file" $ do
    target <- runIO fileMultiLine
    result <- runIO . runStack . runDiagnostics . runReadFSIO $ fingerprint target

    it "fingerprints raw correctly" $
      assertOnSuccess result $ \_ c ->
        (toText . combinedRaw $ c) `shouldBe` "8bd20ac94646806aec6dadb9325218718d1ba8abc039da6abc9aa8159658671d"

    it "fingerprints comment stripped correctly" $
      assertOnSuccess result $ \_ c ->
        case (combinedCommentStripped c) of
          Nothing -> expectationFailure "should have comment strip fingerprinted file"
          Just fp -> (toText fp) `shouldBe` "2f0ac453340cab3a92d68297964634c8a80ca5828f760c588b80a69f963cea68"

  describe "multi line file with comment" $ do
    target <- runIO fileMultiLineComment
    result <- runIO . runStack . runDiagnostics . runReadFSIO $ fingerprint target

    it "fingerprints raw correctly" $
      assertOnSuccess result $ \_ c ->
        (toText . combinedRaw $ c) `shouldBe` "9976b2fda660ce9a9084992881df35ec17bc78b39a2bf24434d8fed875af3a2d"

    it "fingerprints comment stripped correctly" $
      assertOnSuccess result $ \_ c ->
        case (combinedCommentStripped c) of
          Nothing -> expectationFailure "should have comment strip fingerprinted file"
          Just fp -> (toText fp) `shouldBe` "44fc8f68ab633c7ca0240a66e4ff038c0f2412fe69d14b6f052556edaa1b9160"

  describe "multi line file with comment (windows)" $ do
    target <- runIO fileMultiLineCommentWindows
    result <- runIO . runStack . runDiagnostics . runReadFSIO $ fingerprint target

    it "fingerprints raw correctly" $
      assertOnSuccess result $ \_ c ->
        (toText . combinedRaw $ c) `shouldBe` "9976b2fda660ce9a9084992881df35ec17bc78b39a2bf24434d8fed875af3a2d"

    it "fingerprints comment stripped correctly" $
      assertOnSuccess result $ \_ c ->
        case (combinedCommentStripped c) of
          Nothing -> expectationFailure "should have comment strip fingerprinted file"
          Just fp -> (toText fp) `shouldBe` "44fc8f68ab633c7ca0240a66e4ff038c0f2412fe69d14b6f052556edaa1b9160"

fileEmpty :: IO (Path Abs File)
fileEmpty = PIO.resolveFile' "test/App/Fossa/VSI/testdata/empty.c"

fileBinary :: IO (Path Abs File)
fileBinary = PIO.resolveFile' "test/App/Fossa/VSI/testdata/binary"

fileSingleLine :: IO (Path Abs File)
fileSingleLine = PIO.resolveFile' "test/App/Fossa/VSI/testdata/single_line.c"

fileSingleLineComment :: IO (Path Abs File)
fileSingleLineComment = PIO.resolveFile' "test/App/Fossa/VSI/testdata/single_line_comment.c"

fileMultiLine :: IO (Path Abs File)
fileMultiLine = PIO.resolveFile' "test/App/Fossa/VSI/testdata/multi_line.c"

fileMultiLineComment :: IO (Path Abs File)
fileMultiLineComment = PIO.resolveFile' "test/App/Fossa/VSI/testdata/multi_line_comment.c"

fileMultiLineCommentWindows :: IO (Path Abs File)
fileMultiLineCommentWindows = PIO.resolveFile' "test/App/Fossa/VSI/testdata/multi_line_comment_cr.c"
