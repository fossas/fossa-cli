{-# LANGUAGE TemplateHaskell #-}

module Discovery.ArchiveSpec (spec) where

import Conduit (await, runConduitRes, sourceFile, (.|))
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (Diagnostics, fatalText, runDiagnostics)
import Control.Carrier.Finally (runFinally)
import Control.Effect.Exception (IOException, Lift, catch)
import Control.Effect.Lift (sendIO)
import Crypto.Hash (Digest, SHA256, hashFinalize, hashInit, hashUpdate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Conversion (ToText (..))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Discovery.Archive (extractRpm, extractTar, extractTarBz2, extractTarGz, extractTarXz, extractZip, withArchive)
import Discovery.Walk (WalkStep (WalkContinue), walk')
import Effect.ReadFS (ReadFS, runReadFSIO)
import Path (Abs, Dir, File, Path, mkRelDir, mkRelFile, toFilePath, (</>))
import Path.Extra (renderRelative)
import Path.IO qualified as PIO
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "extract zip archive to a temporary location" $ do
    target <- runIO simpleZipPath
    (extractedDir, extractedContentA, extractedContentB) <- runIO $
      runFinally . withArchive extractZip target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)
    tempDirExists <- runIO $ PIO.doesDirExist extractedDir

    it "should have extracted the correct contents" $ do
      extractedContentB `shouldBe` expectedSimpleContentB
      extractedContentA `shouldBe` expectedSimpleContentA

    it "should have cleaned up the temporary directory" $ do
      tempDirExists `shouldBe` False

  describe "extract tar archive to a temporary location" $ do
    target <- runIO simpleTarPath
    (extractedDir, extractedContentA, extractedContentB) <- runIO $
      runFinally . withArchive extractTar target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)
    tempDirExists <- runIO $ PIO.doesDirExist extractedDir

    it "should have extracted the correct contents" $ do
      extractedContentB `shouldBe` expectedSimpleContentB
      extractedContentA `shouldBe` expectedSimpleContentA

    it "should have cleaned up the temporary directory" $ do
      tempDirExists `shouldBe` False

  describe "extract tar.gz archive to a temporary location" $ do
    target <- runIO simpleTarGzPath
    (extractedDir, extractedContentA, extractedContentB) <- runIO $
      runFinally . withArchive extractTarGz target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)
    tempDirExists <- runIO $ PIO.doesDirExist extractedDir

    it "should have extracted the correct contents" $ do
      extractedContentB `shouldBe` expectedSimpleContentB
      extractedContentA `shouldBe` expectedSimpleContentA

    it "should have cleaned up the temporary directory" $ do
      tempDirExists `shouldBe` False

  describe "extract tar.xz archive to a temporary location" $ do
    target <- runIO simpleTarXzPath
    (extractedDir, extractedContentA, extractedContentB) <- runIO $
      runFinally . withArchive extractTarXz target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)
    tempDirExists <- runIO $ PIO.doesDirExist extractedDir

    it "should have extracted the correct contents" $ do
      extractedContentB `shouldBe` expectedSimpleContentB
      extractedContentA `shouldBe` expectedSimpleContentA

    it "should have cleaned up the temporary directory" $ do
      tempDirExists `shouldBe` False

  describe "extract tar.bz2 archive to a temporary location" $ do
    target <- runIO simpleTarBz2Path
    (extractedDir, extractedContentA, extractedContentB) <- runIO $
      runFinally . withArchive extractTarBz2 target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)
    tempDirExists <- runIO $ PIO.doesDirExist extractedDir

    it "should have extracted the correct contents" $ do
      extractedContentB `shouldBe` expectedSimpleContentB
      extractedContentA `shouldBe` expectedSimpleContentA

    it "should have cleaned up the temporary directory" $ do
      tempDirExists `shouldBe` False

  describe "extract el7 rpm to a temporary location" $ do
    target <- runIO rpmCurlEl7Path
    result <- runIO . runFinally . runDiagnostics . runReadFSIO $ withArchive extractRpm target hashFiles

    it "should have extracted the correct contents" $ case result of
      Left _ -> expectationFailure "could not extract rpm"
      Right contents -> contents `shouldBe` rpmCurlEl7ExpectedFiles

  describe "extract fc35 rpm to a temporary location" $ do
    target <- runIO rpmCurlFc35Path
    result <- runIO . runFinally . runDiagnostics . runReadFSIO $ withArchive extractRpm target hashFiles

    it "should have extracted the correct contents" $ case result of
      Left _ -> expectationFailure "could not extract rpm"
      Right contents -> contents `shouldBe` rpmCurlFc35ExpectedFiles

simpleZipPath :: IO (Path Abs File)
simpleZipPath = PIO.resolveFile' "test/Discovery/testdata/simple.zip"

simpleTarPath :: IO (Path Abs File)
simpleTarPath = PIO.resolveFile' "test/Discovery/testdata/simple.tar"

simpleTarGzPath :: IO (Path Abs File)
simpleTarGzPath = PIO.resolveFile' "test/Discovery/testdata/simple.tar.gz"

simpleTarXzPath :: IO (Path Abs File)
simpleTarXzPath = PIO.resolveFile' "test/Discovery/testdata/simple.tar.xz"

simpleTarBz2Path :: IO (Path Abs File)
simpleTarBz2Path = PIO.resolveFile' "test/Discovery/testdata/simple.tar.bz2"

expectedSimpleContentA :: Text
expectedSimpleContentA = "6b5effe3-215a-49ec-9286-f0702f7eb529"

expectedSimpleContentB :: Text
expectedSimpleContentB = "8dea86e4-4365-4711-872b-6f652b02c8d9"

rpmCurlEl7Path :: IO (Path Abs File)
rpmCurlEl7Path = PIO.resolveFile' "test/Discovery/testdata/curl-7.29.0-59.el7.x86_64.rpm"

rpmCurlFc35Path :: IO (Path Abs File)
rpmCurlFc35Path = PIO.resolveFile' "test/Discovery/testdata/curl-7.78.0-3.fc35.x86_64.rpm"

rpmCurlEl7ExpectedFiles :: Map Text Text
rpmCurlEl7ExpectedFiles =
  Map.fromList
    [ ("usr/bin/curl", "47dbba060d72829769ef29ea3675b82e6df47b12b54dee7a09f4801926b74dc9")
    , ("usr/share/man/man1/curl.1.gz", "8b42fd5707727da8ef67636370c20322101f4c5ebe99d999aef185e9acb37ee6")
    , ("usr/share/doc/curl-7.29.0/TheArtOfHttpScripting", "e8726c9baf3cadda2845ecff7e6580db784e84283f698a69293d79156875425c")
    , ("usr/share/doc/curl-7.29.0/FAQ", "f43a2c6f882ccbe3cbbb56ce1f472642644e48adb32d7d73fde31685df2b1785")
    , ("usr/share/doc/curl-7.29.0/CHANGES", "23ca1d732e2523a2b591fd196ec5ddd7daa48a1b4bf59886560878aaea468b99")
    , ("usr/share/doc/curl-7.29.0/RESOURCES", "985a2d39c877b847da64ee73d0e5afa0431af546fe8ffe90cd1882675a87b217")
    , ("usr/share/doc/curl-7.29.0/FEATURES", "a78c0c8a3952e9a2bf3204ecce5ba3fbd0e01f127baf8175cbb8db3865e6b9f0")
    , ("usr/share/doc/curl-7.29.0/BUGS", "24bbe914ac0937906c745115fa9d14b5e7e5cec0332998683d84b76a791d57bb")
    , ("usr/share/doc/curl-7.29.0/README", "5540c522b6d62887dca72ed06345b88b1603a01072418b8fc93a7798b1560359")
    , ("usr/share/doc/curl-7.29.0/TODO", "d47375aff721538403f09e5d1f77583efb3634f5b0538e33cbf8bfaf4584389b")
    , ("usr/share/doc/curl-7.29.0/COPYING", "85a861a77b1c1dd6cbf4b5e4edca2def74af30de7b40c0b05131c32dd66b1081")
    , ("usr/share/doc/curl-7.29.0/MANUAL", "6c3f52d84241d76d371c20be52e22bf4f7fba3fd554f35c23323b5236bc26c32")
    ]

rpmCurlFc35ExpectedFiles :: Map Text Text
rpmCurlFc35ExpectedFiles =
  Map.fromList
    [ ("usr/bin/curl", "f44294f0cb31bdbddd7dde93702a98dc9de60a0c0207f0e7ee6df7631a9ae641")
    , ("usr/lib/.build-id/b3/1694338b7ba8cedd532408473dbac8ebe509c5", "a7d84c7556e2d15f1624df5899d419fbc8c540ff7d39391798fafda603c12325")
    , ("usr/share/man/man1/curl.1.gz", "e3ab38e59cda834a11cee0ae4659dc6d609d8ed1f3e3c80dcd0d28cb56908d4c")
    , ("usr/share/doc/curl/BUGS.md", "c5fc32214134097232490fa9e0d3cd1f299b04f5e550c2bfc8ff081ff29f0836")
    , ("usr/share/doc/curl/FAQ", "d00231e857aa821f9ca1519681463fafea852bb437c9b0ca49ab341bdee04b55")
    , ("usr/share/doc/curl/CHANGES", "0ab7f82274290a06b6a5d78dab3097c8a589d1f77325e64be32599c522b7dd96")
    , ("usr/share/doc/curl/TheArtOfHttpScripting.md", "600d0796844ccf177b452d2f3abed65eb1f454c2685381d479f76c2dafc83789")
    , ("usr/share/doc/curl/README", "ce118b51897f4452dcbe7d2042f05222fd2a8c0362ca177b3cd6c6fb3a335548")
    , ("usr/share/doc/curl/TODO", "06e052269d2ec3f08b65e257d7b65609e3b678c0e2143c4410ca667c097baa2b")
    , ("usr/share/doc/curl/FEATURES.md", "ceecb9363eb82c80a7096064d01f89abf2149c3e66b92674966f2707dd10b83a")
    , ("usr/share/zsh/site-functions/_curl", "ee8fe9041235e96a89c66ab3accc6f6bb6a9cc473566031c51fb0553d830f258")
    ]

-- | Walk the provided dir, turning each file found into a `(relative path, base16 sha256)` tuple, then merge them into a map.
-- Playing fast and loose with these types since this is local to the test and it makes things easier in such a constrained context.
--
-- Future reader, if you're using this for inspiration, please use better formed types.
hashFiles :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Path Abs Dir -> m (Map Text Text)
hashFiles dir = do
  results <- walk' collect dir
  pure . Map.fromList $ fmap mkRel results
  where
    mkRel (p, a) = (renderRelative dir p, a)
    collect _ _ files = do
      fps <- traverse hashFilePair files
      pure (fps, WalkContinue)

-- | Adapted from fingerprint hashing, but kept separate so that if the fingerprint implementation changes we don't see errors here.
-- TODO: Maybe basic file hashing should be in ReadFS?
hashFilePair :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m (Path Abs File, Text)
hashFilePair file = do
  (h :: Digest SHA256) <- hashFile $ toFilePath file
  pure (file, toText . show $ h)
  where
    sinkHash = sink hashInit
    hashFile fp =
      sendIO (runConduitRes (sourceFile fp .| sinkHash))
        `catch` (\(e :: IOException) -> fatalText ("unable to hash file: " <> toText (show e)))
    sink ctx = do
      b <- await
      case b of
        Nothing -> return $! hashFinalize ctx
        Just bs -> sink $! hashUpdate ctx bs
