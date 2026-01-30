{-# LANGUAGE TemplateHaskell #-}

module Discovery.ArchiveSpec (spec) where

import Conduit (runConduitRes, sourceFile, (.|))
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (Diagnostics, fatal, fatalOnIOException, fatalText, runDiagnostics)
import Control.Carrier.Finally (runFinally)
import Control.Carrier.Stack (runStack)
import Control.Effect.Exception (Lift)
import Control.Effect.Lift (sendIO)
import Crypto.Hash (Digest, SHA256)
import Data.Conduit.Extra (sinkHash)
import Data.List (isInfixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Conversion (ToText (..))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Discovery.Archive (extractRpm, extractTar, extractTarBz2, extractTarGz, extractTarXz, extractZip, withArchive)
import Discovery.Walk (WalkStep (WalkContinue), walk')
import Effect.ReadFS (ReadFS, runReadFSIO)
import Path (Abs, Dir, File, Path, Rel, SomeBase (Abs, Rel), mkRelDir, mkRelFile, toFilePath, (</>))
import Path.Extra (tryMakeRelative)
import Path.IO qualified as PIO
import ResultUtil
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldSatisfy)

failOnMaybe :: (Has Diagnostics sig m) => String -> m (Maybe a) -> m a
failOnMaybe s m = maybe (fatal s) pure =<< m

spec :: Spec
spec = do
  describe "extract zip archive to a temporary location" $ do
    target <- runIO simpleZipPath
    result <- runIO $
      runStack . runDiagnostics . runFinally . failOnMaybe "extractZip" . withArchive extractZip target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)

    it "should have extracted the correct contents" $ do
      assertOnSuccess result $ \_ (_, _, extractedContentB) -> extractedContentB `shouldBe` expectedSimpleContentB
      assertOnSuccess result $ \_ (_, extractedContentA, _) -> extractedContentA `shouldBe` expectedSimpleContentA

    it "should have cleaned up the temporary directory" $ do
      assertOnSuccess result $ \_ (extractedDir, _, _) -> do
        tempDirExists <- sendIO $ PIO.doesDirExist extractedDir
        tempDirExists `shouldBe` False

  describe "extract tar archive to a temporary location" $ do
    target <- runIO simpleTarPath
    result <- runIO $
      runStack . runDiagnostics . runFinally . failOnMaybe "extractTar" . withArchive extractTar target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)

    emptyTarget <- runIO emptyTarPath
    emptyResult <- runIO . runStack . runDiagnostics . runFinally $ withArchive extractTar emptyTarget (pure . const ())

    it "should have extracted the correct contents" $ do
      assertOnSuccess result $ \_ (_, _, extractedContentB) -> extractedContentB `shouldBe` expectedSimpleContentB
      assertOnSuccess result $ \_ (_, extractedContentA, _) -> extractedContentA `shouldBe` expectedSimpleContentA

    it "should have cleaned up the temporary directory" $ do
      assertOnSuccess result $ \_ (extractedDir, _, _) -> do
        tempDirExists <- sendIO $ PIO.doesDirExist extractedDir
        tempDirExists `shouldBe` False

    it "should not thrown an error when working with empty tar file" $ do
      assertOnSuccess emptyResult $ \warns _ -> (length warns) `shouldBe` 0

  describe "extract tar.gz archive to a temporary location" $ do
    target <- runIO simpleTarGzPath
    result <- runIO $
      runStack . runDiagnostics . runFinally . failOnMaybe "extractTarGz" . withArchive extractTarGz target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)

    it "should have extracted the correct contents" $ do
      assertOnSuccess result $ \_ (_, _, extractedContentB) -> extractedContentB `shouldBe` expectedSimpleContentB
      assertOnSuccess result $ \_ (_, extractedContentA, _) -> extractedContentA `shouldBe` expectedSimpleContentA

    it "should have cleaned up the temporary directory" $ do
      assertOnSuccess result $ \_ (extractedDir, _, _) -> do
        tempDirExists <- sendIO $ PIO.doesDirExist extractedDir
        tempDirExists `shouldBe` False

  describe "extract tar.xz archive to a temporary location" $ do
    target <- runIO simpleTarXzPath
    result <- runIO $
      runStack . runDiagnostics . runFinally . failOnMaybe "extractTarXz" . withArchive extractTarXz target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)

    it "should have extracted the correct contents" $ do
      assertOnSuccess result $ \_ (_, _, extractedContentB) -> extractedContentB `shouldBe` expectedSimpleContentB
      assertOnSuccess result $ \_ (_, extractedContentA, _) -> extractedContentA `shouldBe` expectedSimpleContentA

    it "should have cleaned up the temporary directory" $ do
      assertOnSuccess result $ \_ (extractedDir, _, _) -> do
        tempDirExists <- sendIO $ PIO.doesDirExist extractedDir
        tempDirExists `shouldBe` False

  -- Regression test for ANE-2716: tar archives with GNU long file name entries (type 'L')
  -- for symbolic links would fail with TwoTypeLEntries error because removeTarLinks
  -- removed the symlinks but left orphaned L entries.
  --
  -- GNU tar uses type 'L' entries to store paths exceeding 100 characters, and type 'K'
  -- entries for long link targets. Each L/K entry is followed by the actual file entry.
  -- Reference: https://www.gnu.org/software/tar/manual/html_node/Standard.html
  --   (search for "typeflag" - 'L' and 'K' are GNU extensions listed in the table)
  describe "extract tar.xz archive with GNU long file name entries for symlinks" $ do
    target <- runIO longFileNameSymlinkTarXzPath
    result <- runIO $
      runStack . runDiagnostics . runFinally . failOnMaybe "extractTarXz" . withArchive extractTarXz target $ \dir -> do
        -- The symlinks are removed by removeTarLinks, but the target file should exist
        content <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "symlink-longname-test") </> $(mkRelFile "target.txt")
        pure (dir, content)

    it "should extract without TwoTypeLEntries error" $ do
      assertOnSuccess result $ \_ (_, extractedContent) -> extractedContent `shouldBe` "target content\n"

    it "should have cleaned up the temporary directory" $ do
      assertOnSuccess result $ \_ (extractedDir, _) -> do
        tempDirExists <- sendIO $ PIO.doesDirExist extractedDir
        tempDirExists `shouldBe` False

  describe "extract tar.bz2 archive to a temporary location" $ do
    target <- runIO simpleTarBz2Path
    result <- runIO $
      runStack . runDiagnostics . runFinally . failOnMaybe "extractZip" . withArchive extractTarBz2 target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)

    it "should have extracted the correct contents" $ do
      assertOnSuccess result $ \_ (_, _, extractedContentB) -> extractedContentB `shouldBe` expectedSimpleContentB
      assertOnSuccess result $ \_ (_, extractedContentA, _) -> extractedContentA `shouldBe` expectedSimpleContentA

    it "should have cleaned up the temporary directory" $ do
      assertOnSuccess result $ \_ (extractedDir, _, _) -> do
        tempDirExists <- sendIO $ PIO.doesDirExist extractedDir
        tempDirExists `shouldBe` False

  describe "extract el7 (xz) rpm to a temporary location" $ do
    target <- runIO rpmCurlEl7Path
    result <- runIO . runStack . runFinally . runDiagnostics . runReadFSIO . failOnMaybe "extractZip" $ withArchive extractRpm target hashFiles

    it "should have extracted the correct contents" $
      assertOnSuccess result $
        \_ contents -> contents `shouldBe` rpmCurlEl7ExpectedFiles

  describe "extract fc35 (zstd) rpm to a temporary location" $ do
    target <- runIO rpmCurlFc35Path
    result <- runIO . runStack . runFinally . runDiagnostics . runReadFSIO . failOnMaybe "extractZip" $ withArchive extractRpm target hashFiles

    it "should have extracted the correct contents" $
      assertOnSuccess result $
        \_ contents -> contents `shouldBe` rpmCurlFc35ExpectedFiles

  describe "Archive unpack failures" $ do
    target <- runIO brokenZipPath
    result <- runIO . runStack . runDiagnostics . runFinally $ withArchive extractZip target (pure . const ())

    it "Succeeds with a warning on archive unpack failure" $ do
      assertOnSuccess result $ \warnings _ ->
        warnings `shouldSatisfy` any (\warning -> "Parsing of archive structure failed" `isInfixOf` show warning)

simpleZipPath :: IO (Path Abs File)
simpleZipPath = PIO.resolveFile' "test/Discovery/testdata/simple.zip"

brokenZipPath :: IO (Path Abs File)
brokenZipPath = PIO.resolveFile' "test/Discovery/testdata/broken.zip"

simpleTarPath :: IO (Path Abs File)
simpleTarPath = PIO.resolveFile' "test/Discovery/testdata/simple.tar"

emptyTarPath :: IO (Path Abs File)
emptyTarPath = PIO.resolveFile' "test/Discovery/testdata/empty.tar"

simpleTarGzPath :: IO (Path Abs File)
simpleTarGzPath = PIO.resolveFile' "test/Discovery/testdata/simple.tar.gz"

simpleTarXzPath :: IO (Path Abs File)
simpleTarXzPath = PIO.resolveFile' "test/Discovery/testdata/simple.tar.xz"

simpleTarBz2Path :: IO (Path Abs File)
simpleTarBz2Path = PIO.resolveFile' "test/Discovery/testdata/simple.tar.bz2"

-- Archive with GNU long file name entries (type 'L') for symlinks.
-- Used to test regression fix for ANE-2716 (TwoTypeLEntries error).
--
-- GNU tar uses type 'L' entries to store paths exceeding 100 characters.
-- Reference: https://www.gnu.org/software/tar/manual/html_node/Standard.html
--   (search for "typeflag" - 'L' is a GNU extension for long names)
--
-- Created with:
--   mkdir -p symlink-longname-test/very/deeply/nested/directory/structure/that/exceeds/one/hundred/characters/in/total/path/length
--   echo "target content" > symlink-longname-test/target.txt
--   for i in 1 2 3; do
--     ln -s ../../../../../../../../../../../target.txt \
--       "symlink-longname-test/very/deeply/nested/directory/structure/that/exceeds/one/hundred/characters/in/total/path/length/symlink$i.txt"
--   done
--   gtar --format=gnu -cvf symlink-longname.tar symlink-longname-test && xz symlink-longname.tar
--   rm -rf symlink-longname-test
longFileNameSymlinkTarXzPath :: IO (Path Abs File)
longFileNameSymlinkTarXzPath = PIO.resolveFile' "test/Discovery/testdata/symlink-longname.tar.xz"

expectedSimpleContentA :: Text
expectedSimpleContentA = "6b5effe3-215a-49ec-9286-f0702f7eb529"

expectedSimpleContentB :: Text
expectedSimpleContentB = "8dea86e4-4365-4711-872b-6f652b02c8d9"

rpmCurlEl7Path :: IO (Path Abs File)
rpmCurlEl7Path = PIO.resolveFile' "test/Discovery/testdata/curl-7.29.0-59.el7.x86_64.rpm"

rpmCurlFc35Path :: IO (Path Abs File)
rpmCurlFc35Path = PIO.resolveFile' "test/Discovery/testdata/curl-7.78.0-3.fc35.x86_64.rpm"

rpmCurlEl7ExpectedFiles :: Map (Path Rel File) Text
rpmCurlEl7ExpectedFiles =
  Map.fromList
    [ ($(mkRelFile "usr/bin/curl"), "47dbba060d72829769ef29ea3675b82e6df47b12b54dee7a09f4801926b74dc9")
    , ($(mkRelFile "usr/share/man/man1/curl.1.gz"), "8b42fd5707727da8ef67636370c20322101f4c5ebe99d999aef185e9acb37ee6")
    , ($(mkRelFile "usr/share/doc/curl-7.29.0/TheArtOfHttpScripting"), "e8726c9baf3cadda2845ecff7e6580db784e84283f698a69293d79156875425c")
    , ($(mkRelFile "usr/share/doc/curl-7.29.0/FAQ"), "f43a2c6f882ccbe3cbbb56ce1f472642644e48adb32d7d73fde31685df2b1785")
    , ($(mkRelFile "usr/share/doc/curl-7.29.0/CHANGES"), "23ca1d732e2523a2b591fd196ec5ddd7daa48a1b4bf59886560878aaea468b99")
    , ($(mkRelFile "usr/share/doc/curl-7.29.0/RESOURCES"), "985a2d39c877b847da64ee73d0e5afa0431af546fe8ffe90cd1882675a87b217")
    , ($(mkRelFile "usr/share/doc/curl-7.29.0/FEATURES"), "a78c0c8a3952e9a2bf3204ecce5ba3fbd0e01f127baf8175cbb8db3865e6b9f0")
    , ($(mkRelFile "usr/share/doc/curl-7.29.0/BUGS"), "24bbe914ac0937906c745115fa9d14b5e7e5cec0332998683d84b76a791d57bb")
    , ($(mkRelFile "usr/share/doc/curl-7.29.0/README"), "5540c522b6d62887dca72ed06345b88b1603a01072418b8fc93a7798b1560359")
    , ($(mkRelFile "usr/share/doc/curl-7.29.0/TODO"), "d47375aff721538403f09e5d1f77583efb3634f5b0538e33cbf8bfaf4584389b")
    , ($(mkRelFile "usr/share/doc/curl-7.29.0/COPYING"), "85a861a77b1c1dd6cbf4b5e4edca2def74af30de7b40c0b05131c32dd66b1081")
    , ($(mkRelFile "usr/share/doc/curl-7.29.0/MANUAL"), "6c3f52d84241d76d371c20be52e22bf4f7fba3fd554f35c23323b5236bc26c32")
    ]

rpmCurlFc35ExpectedFiles :: Map (Path Rel File) Text
rpmCurlFc35ExpectedFiles =
  Map.fromList
    [ ($(mkRelFile "usr/bin/curl"), "f44294f0cb31bdbddd7dde93702a98dc9de60a0c0207f0e7ee6df7631a9ae641")
    , ($(mkRelFile "usr/lib/.build-id/b3/1694338b7ba8cedd532408473dbac8ebe509c5"), "a7d84c7556e2d15f1624df5899d419fbc8c540ff7d39391798fafda603c12325")
    , ($(mkRelFile "usr/share/man/man1/curl.1.gz"), "e3ab38e59cda834a11cee0ae4659dc6d609d8ed1f3e3c80dcd0d28cb56908d4c")
    , ($(mkRelFile "usr/share/doc/curl/BUGS.md"), "c5fc32214134097232490fa9e0d3cd1f299b04f5e550c2bfc8ff081ff29f0836")
    , ($(mkRelFile "usr/share/doc/curl/FAQ"), "d00231e857aa821f9ca1519681463fafea852bb437c9b0ca49ab341bdee04b55")
    , ($(mkRelFile "usr/share/doc/curl/CHANGES"), "0ab7f82274290a06b6a5d78dab3097c8a589d1f77325e64be32599c522b7dd96")
    , ($(mkRelFile "usr/share/doc/curl/TheArtOfHttpScripting.md"), "600d0796844ccf177b452d2f3abed65eb1f454c2685381d479f76c2dafc83789")
    , ($(mkRelFile "usr/share/doc/curl/README"), "ce118b51897f4452dcbe7d2042f05222fd2a8c0362ca177b3cd6c6fb3a335548")
    , ($(mkRelFile "usr/share/doc/curl/TODO"), "06e052269d2ec3f08b65e257d7b65609e3b678c0e2143c4410ca667c097baa2b")
    , ($(mkRelFile "usr/share/doc/curl/FEATURES.md"), "ceecb9363eb82c80a7096064d01f89abf2149c3e66b92674966f2707dd10b83a")
    , ($(mkRelFile "usr/share/zsh/site-functions/_curl"), "ee8fe9041235e96a89c66ab3accc6f6bb6a9cc473566031c51fb0553d830f258")
    ]

-- | Walk the provided dir, turning each file found into a `(relative path, base16 sha256)` tuple, then merge them into a map.
-- Playing fast and loose with these types since this is local to the test and it makes things easier in such a constrained context.
--
-- Future reader, if you're using this for inspiration, please use better formed types.
hashFiles :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Path Abs Dir -> m (Map (Path Rel File) Text)
hashFiles dir = do
  results <- walk' collect dir
  asRel <- traverse mkRel results
  pure $ Map.fromList asRel
  where
    mkRel (p, a) = case tryMakeRelative dir p of
      Rel f -> pure (f, a)
      Abs _ -> fatalText $ "Unable to make " <> toText p <> " relative to '" <> toText dir <> "'"
    hashToPair path = do
      hash <- hashFile path
      pure (path, toText . show $ hash)
    collect _ _ files = do
      fps <- traverse hashToPair files
      pure (fps, WalkContinue)

-- | Adapted from fingerprint hashing, but kept separate so that if the fingerprint implementation changes we don't see errors here.
-- TODO: Maybe basic file hashing should be in ReadFS?
hashFile :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m (Digest SHA256)
hashFile file = (fatalOnIOException "hash file") . sendIO . runConduitRes $ sourceFile (toFilePath file) .| sinkHash
