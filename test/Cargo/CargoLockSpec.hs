module Cargo.CargoLockSpec (
  spec,
) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Strategy.CargoLock
import Test.Hspec
import Test.Hspec qualified as H

spec :: H.Spec
spec = do
  v3 <- H.runIO (TIO.readFile "test/Cargo/testdata/cargo_lock_v3.toml")
  v4 <- H.runIO (TIO.readFile "test/Cargo/testdata/cargo_lock_v4.toml")

  v3Spec v3
  v4Spec v4
  depStringSpec
  errorSpec
  newerVersionSpec

-- | Registry source shared by the fixtures.
registrySource :: Text
registrySource = "registry+https://github.com/rust-lang/crates.io-index"

sparseSource :: Text
sparseSource = "sparse+https://index.crates.io/"

gitDepSourceV3 :: Text
gitDepSourceV3 = "git+https://github.com/example/git_dep?tag=v0.5.0#7e0838891c303123082d623f9d0a5a3b05b0e9c8"

gitDepASource :: Text
gitDepASource = "git+https://github.com/example/git_dep_a?tag=v1.0.0#abc123def456789012345678901234567890abcd"

gitDepBSource :: Text
gitDepBSource = "git+https://github.com/example/git_dep_b#def456abc7890123456789012345678901234abcd"

mkDep :: Text -> Maybe Text -> CargoDependency
mkDep = CargoDependency

mkPkg :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> [CargoDependency] -> CargoPackage
mkPkg name version source checksum deps =
  CargoPackage
    { packageName = name
    , packageVersion = version
    , packageSource = source
    , packageChecksum = checksum
    , packageDependencies = deps
    }

-- | Full expected structure of testdata/cargo_lock_v3.toml.
expectedV3 :: CargoLock
expectedV3 =
  CargoLock
    { lockVersion = 3
    , lockWarnings = []
    , lockPackages =
        [ mkPkg
            "my_project"
            (Just "0.1.0")
            Nothing
            Nothing
            [ mkDep "clap" (Just "2.33.0")
            , mkDep "my_local_lib" Nothing
            ]
        , mkPkg
            "clap"
            (Just "2.33.0")
            (Just registrySource)
            (Just "b1f5388f38ba8b63a4c1c94a0f11945f67330b630664e38827200af0852c104a")
            [ mkDep "ansi_term" (Just "0.11.0")
            , mkDep "winapi" (Just "0.3.8")
            ]
        , mkPkg
            "winapi"
            (Just "0.3.8")
            (Just sparseSource)
            (Just "10429ff5b272cb971f0d6376d2aa6691cb76d6cd1601980bd2893f432c117bc0")
            []
        , mkPkg "git_dep" (Just "0.5.0") (Just gitDepSourceV3) Nothing []
        , mkPkg
            "my_local_lib"
            (Just "0.1.0")
            Nothing
            Nothing
            [mkDep "clap" (Just "2.33.0")]
        ]
    }

-- | Full expected structure of testdata/cargo_lock_v4.toml.
expectedV4 :: CargoLock
expectedV4 =
  CargoLock
    { lockVersion = 4
    , lockWarnings = []
    , lockPackages =
        [ mkPkg
            "my_project"
            (Just "0.1.0")
            Nothing
            Nothing
            [ mkDep "clap" (Just "2.33.0")
            , mkDep "git_dep_a" Nothing
            , mkDep "git_dep_b" Nothing
            ]
        , mkPkg
            "clap"
            (Just "2.33.0")
            (Just registrySource)
            (Just "b1f5388f38ba8b63a4c1c94a0f11945f67330b630664e38827200af0852c104a")
            [mkDep "ansi_term" (Just "0.12.1")]
        , mkPkg
            "ansi_term"
            (Just "0.12.1")
            (Just registrySource)
            (Just "5079b84978794566068f994c4061d92d0f3e33f98475a0681a9b8a7d2e99c99d")
            []
        , mkPkg "git_dep_a" (Just "1.0.0") (Just gitDepASource) Nothing []
        , mkPkg "git_dep_b" Nothing (Just gitDepBSource) Nothing []
        ]
    }

v3Spec :: Text -> Spec
v3Spec contents = do
  let result = parseCargoLock contents
  describe "parseCargoLock (v3 fixture)" $ do
    it "should parse into the expected structure" $
      result `shouldBe` Right expectedV3

    it "should have no warnings for a supported version" $
      case result of
        Right lock -> lockWarnings lock `shouldBe` []
        Left err -> expectationFailure $ "unexpected error: " <> show err

    it "should mark local packages as having no source" $
      case lookupPkg result "my_local_lib" of
        Just pkg -> packageSource pkg `shouldBe` Nothing
        Nothing -> expectationFailure "my_local_lib not found"

    it "should give local packages no checksum" $
      case lookupPkg result "my_local_lib" of
        Just pkg -> packageChecksum pkg `shouldBe` Nothing
        Nothing -> expectationFailure "my_local_lib not found"

    it "should default missing dependencies arrays to empty" $
      case lookupPkg result "winapi" of
        Just pkg -> packageDependencies pkg `shouldBe` []
        Nothing -> expectationFailure "winapi not found"

    it "should parse git sources verbatim" $
      case lookupPkg result "git_dep" of
        Just pkg -> packageSource pkg `shouldBe` Just gitDepSourceV3
        Nothing -> expectationFailure "git_dep not found"

v4Spec :: Text -> Spec
v4Spec contents = do
  let result = parseCargoLock contents
  describe "parseCargoLock (v4 fixture)" $ do
    it "should parse into the expected structure" $
      result `shouldBe` Right expectedV4

    it "should keep unversioned git packages at version Nothing" $
      case lookupPkg result "git_dep_b" of
        Just pkg -> packageVersion pkg `shouldBe` Nothing
        Nothing -> expectationFailure "git_dep_b not found"

    it "should keep versioned git packages at their version" $
      case lookupPkg result "git_dep_a" of
        Just pkg -> packageVersion pkg `shouldBe` Just "1.0.0"
        Nothing -> expectationFailure "git_dep_a not found"

    it "should surface no warnings" $
      case result of
        Right lock -> lockWarnings lock `shouldBe` []
        Left err -> expectationFailure $ "unexpected error: " <> show err

depStringSpec :: Spec
depStringSpec =
  describe "parseDependencyString" $ do
    it "should parse a bare name" $
      parseDependencyString "ansi_term"
        `shouldBe` CargoDependency "ansi_term" Nothing

    it "should parse a disambiguated name + version" $
      parseDependencyString "ansi_term 0.12.1"
        `shouldBe` CargoDependency "ansi_term" (Just "0.12.1")

    it "should split on the last space when there is more than one" $
      parseDependencyString "my dep 1.0.0"
        `shouldBe` CargoDependency "my dep" (Just "1.0.0")

errorSpec :: Spec
errorSpec = do
  describe "parseCargoLock errors" $ do
    let version2 = "version = 2\n\n[[package]]\nname = \"x\"\nversion = \"1.0.0\"\n"
        noVersion = "[[package]]\nname = \"x\"\nversion = \"1.0.0\"\n"
        malformed = "version = 3\n\n[[package]]\nname = "

    it "should reject version 2 as unsupported" $
      parseCargoLock version2 `shouldBe` Left (UnsupportedVersion 2)

    it "should reject version 1 as unsupported" $
      parseCargoLock "version = 1\n\n[[package]]\nname = \"x\"\nversion = \"1.0.0\"\n"
        `shouldBe` Left (UnsupportedVersion 1)

    it "should reject a missing version field" $
      parseCargoLock noVersion `shouldBe` Left (UnsupportedVersion 0)

    it "should surface malformed TOML as a parse error" $
      parseCargoLock malformed `shouldSatisfy` isTomlParseError

newerVersionSpec :: Spec
newerVersionSpec = do
  let v9 =
        "version = 9\n\n[[package]]\n"
          <> "name = \"my_project\"\n"
          <> "version = \"0.1.0\"\n"
          <> "dependencies = [\"clap 2.33.0\"]\n"
  describe "parseCargoLock (version > 4)" $ do
    it "should parse a v9 lockfile successfully" $
      parseCargoLock v9
        `shouldSatisfy` \case
          Right lock -> lockVersion lock == 9
          Left _ -> False

    it "should surface a warning for a newer-than-known version" $
      case parseCargoLock v9 of
        Right lock -> lockWarnings lock `shouldNotBe` []
        Left err -> expectationFailure $ "unexpected error: " <> show err

lookupPkg :: Either CargoLockError CargoLock -> Text -> Maybe CargoPackage
lookupPkg (Right lock) name = findPkg (lockPackages lock) name
lookupPkg (Left _) _ = Nothing

findPkg :: [CargoPackage] -> Text -> Maybe CargoPackage
findPkg packages name =
  foldr
    (\pkg acc -> if isJust acc then acc else if packageName pkg == name then Just pkg else Nothing)
    Nothing
    packages

isTomlParseError :: Either CargoLockError CargoLock -> Bool
isTomlParseError (Left (TomlParseError _)) = True
isTomlParseError _ = False
