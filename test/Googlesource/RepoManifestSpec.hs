{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Googlesource.RepoManifestSpec (
  spec,
) where

import Control.Carrier.Diagnostics hiding (withResult)
import Control.Carrier.Stack (runStack)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text.IO qualified as TIO
import DepTypes
import Effect.ReadFS
import GraphUtil
import Parse.XML
import Path
import Path.IO (getCurrentDir)
import ResultUtil
import Strategy.Googlesource.RepoManifest
import Test.Hspec
import Text.URI.QQ

-- <remote name="aosp" fetch="https://android.googlesource.com" />
remoteOne :: ManifestRemote
remoteOne =
  ManifestRemote
    { remoteName = "aosp"
    , remoteFetch = "https://android.googlesource.com"
    , remoteRevision = Nothing
    }

-- <remote fetch="https://android.othersource.com" name="othersource" revision="google/android-6.0.1_r74" />
remoteTwo :: ManifestRemote
remoteTwo =
  ManifestRemote
    { remoteName = "othersource"
    , remoteFetch = "https://android.othersource.com"
    , remoteRevision = Just "google/android-6.0.1_r74"
    }

basicRemoteList :: [ManifestRemote]
basicRemoteList = [remoteOne, remoteTwo]

-- <default revision="refs/tags/android-10.0.0_r29"
--          remote="aosp"
--          sync-j="4" />
basicDefault :: ManifestDefault
basicDefault =
  ManifestDefault
    { defaultRemote = Just "aosp"
    , defaultRevision = Just "refs/tags/android-10.0.0_r29"
    }

-- <project path="art" name="platform/art" groups="pdk" />
projectOne :: ManifestProject
projectOne =
  ManifestProject
    { projectName = "platform/art"
    , projectPath = Just "art"
    , projectRevision = Nothing
    , projectRemote = Just "aosp"
    }

-- <project path="bionic" name="platform/bionic" groups="pdk" revision="57b7d1574276f5e7f895c884df29f45859da74b6" />
projectTwo :: ManifestProject
projectTwo =
  ManifestProject
    { projectName = "platform/bionic"
    , projectPath = Just "bionic"
    , projectRevision = Just "57b7d1574276f5e7f895c884df29f45859da74b6"
    , projectRemote = Nothing
    }

-- <project path="bionic" name="platform/bionic" groups="pdk" revision="57b7d1574276f5e7f895c884df29f45859da74b6" remote="aosp" />
projectTwoWithRemote :: ManifestProject
projectTwoWithRemote =
  ManifestProject
    { projectName = "platform/bionic"
    , projectPath = Just "bionic"
    , projectRevision = Just "57b7d1574276f5e7f895c884df29f45859da74b6"
    , projectRemote = Just "aosp"
    }

-- <project path="bootable/recovery" name="platform/bootable/recovery" groups="pdk" remote="othersource" />
projectThree :: ManifestProject
projectThree =
  ManifestProject
    { projectName = "platform/bootable/recovery"
    , projectPath = Just "bootable/recovery"
    , projectRevision = Nothing
    , projectRemote = Just "othersource"
    }

-- <project path="cts" name="platform/cts" groups="cts,pdk-cw-fs,pdk-fs" remote="othersource" revision="1111"/>
projectFour :: ManifestProject
projectFour =
  ManifestProject
    { projectName = "platform/cts"
    , projectPath = Just "cts"
    , projectRevision = Just "1111"
    , projectRemote = Just "othersource"
    }

-- <project path="dalvik" name="platform/dalvik" groups="pdk-cw-fs,pdk-fs" />
projectFive :: ManifestProject
projectFive =
  ManifestProject
    { projectName = "platform/dalvik"
    , projectPath = Just "dalvik"
    , projectRevision = Nothing
    , projectRemote = Nothing
    }

basicProjectList :: [ManifestProject]
basicProjectList = [projectOne, projectTwo, projectThree, projectFour, projectFive]

dependencyOne :: Dependency
dependencyOne =
  Dependency
    { dependencyType = GooglesourceType
    , dependencyName = "platform/art"
    , dependencyVersion = Just (CEq "refs/tags/android-10.0.0_r29")
    , dependencyLocations = ["https://android.googlesource.com/platform/art"]
    , dependencyTags = Map.empty
    , dependencyEnvironments = Set.singleton EnvProduction
    }

dependencyTwo :: Dependency
dependencyTwo =
  Dependency
    { dependencyType = GooglesourceType
    , dependencyName = "platform/bionic"
    , dependencyVersion = Just (CEq "57b7d1574276f5e7f895c884df29f45859da74b6")
    , dependencyLocations = ["https://android.googlesource.com/platform/bionic"]
    , dependencyTags = Map.empty
    , dependencyEnvironments = Set.singleton EnvProduction
    }

dependencyThree :: Dependency
dependencyThree =
  Dependency
    { dependencyType = GooglesourceType
    , dependencyName = "platform/bootable/recovery"
    , dependencyVersion = Just (CEq "google/android-6.0.1_r74")
    , dependencyLocations = ["https://android.othersource.com/platform/bootable/recovery"]
    , dependencyTags = Map.empty
    , dependencyEnvironments = Set.singleton EnvProduction
    }

dependencyFour :: Dependency
dependencyFour =
  Dependency
    { dependencyType = GooglesourceType
    , dependencyName = "platform/cts"
    , dependencyVersion = Just (CEq "1111")
    , dependencyLocations = ["https://android.othersource.com/platform/cts"]
    , dependencyTags = Map.empty
    , dependencyEnvironments = Set.singleton EnvProduction
    }

dependencyFive :: Dependency
dependencyFive =
  Dependency
    { dependencyType = GooglesourceType
    , dependencyName = "platform/dalvik"
    , dependencyVersion = Just (CEq "refs/tags/android-10.0.0_r29")
    , dependencyLocations = ["https://android.googlesource.com/platform/dalvik"]
    , dependencyTags = Map.empty
    , dependencyEnvironments = Set.singleton EnvProduction
    }

validatedProjectOne :: ValidatedProject
validatedProjectOne =
  ValidatedProject
    { validatedProjectName = "platform/art"
    , validatedProjectPath = "art"
    , validatedProjectUrl = [uri|https://android.googlesource.com/platform/art|]
    , validatedProjectRevision = "refs/tags/android-10.0.0_r29"
    }

validatedProjectTwo :: ValidatedProject
validatedProjectTwo =
  ValidatedProject
    { validatedProjectName = "platform/bionic"
    , validatedProjectPath = "bionic"
    , validatedProjectUrl = [uri|https://android.googlesource.com/platform/bionic|]
    , validatedProjectRevision = "57b7d1574276f5e7f895c884df29f45859da74b6"
    }

validatedProjectThree :: ValidatedProject
validatedProjectThree =
  ValidatedProject
    { validatedProjectName = "platform/bootable/recovery"
    , validatedProjectPath = "bootable/recovery"
    , validatedProjectUrl = [uri|https://android.othersource.com/platform/bootable/recovery|]
    , validatedProjectRevision = "google/android-6.0.1_r74"
    }

validatedProjectFour :: ValidatedProject
validatedProjectFour =
  ValidatedProject
    { validatedProjectName = "platform/cts"
    , validatedProjectPath = "cts"
    , validatedProjectUrl = [uri|https://android.othersource.com/platform/cts|]
    , validatedProjectRevision = "1111"
    }

validatedProjectFive :: ValidatedProject
validatedProjectFive =
  ValidatedProject
    { validatedProjectName = "platform/dalvik"
    , validatedProjectPath = "dalvik"
    , validatedProjectUrl = [uri|https://android.googlesource.com/platform/dalvik|]
    , validatedProjectRevision = "refs/tags/android-10.0.0_r29"
    }

validatedProjectList :: [ValidatedProject]
validatedProjectList = [validatedProjectOne, validatedProjectTwo, validatedProjectThree, validatedProjectFour, validatedProjectFive]

spec :: Spec
spec = do
  let runIt = runIO . runStack . runDiagnostics . runReadFSIO
  currentDir <- runIO getCurrentDir
  basicManifest <- runIO (TIO.readFile "test/Googlesource/testdata/manifest.xml")
  noDefaultRemoteManifest <- runIO (TIO.readFile "test/Googlesource/testdata/manifest-no-default-remote.xml")
  noDefaultRevisionManifest <- runIO (TIO.readFile "test/Googlesource/testdata/manifest-no-default-revision.xml")

  describe "repo manifest analyzer" $ do
    describe "for a sane manifest" $
      it "reads a file and constructs a dependency list" $
        case parseXML basicManifest of
          Right manifest -> do
            manifestProjects manifest `shouldMatchList` basicProjectList
            manifestDefault manifest `shouldBe` Just basicDefault
            manifestRemotes manifest `shouldMatchList` basicRemoteList
          Left err -> expectationFailure (toString ("could not parse repo manifest file: " <> xmlErrorPretty err))

    describe "for a manifest with no default remote" $
      it "reads a file and constructs a dependency list" $
        case parseXML noDefaultRemoteManifest of
          Right manifest -> do
            manifestProjects manifest `shouldMatchList` [projectOne, projectTwoWithRemote, projectThree, projectFour, projectFive]
            manifestRemotes manifest `shouldMatchList` basicRemoteList
          Left err -> expectationFailure (toString ("could not parse repo manifest file: " <> xmlErrorPretty err))

    describe "for a manifest with no default revision" $
      it "reads a file and constructs a dependency list" $
        case parseXML noDefaultRevisionManifest of
          Right manifest -> do
            manifestProjects manifest `shouldMatchList` basicProjectList
            manifestRemotes manifest `shouldMatchList` basicRemoteList
          Left err -> expectationFailure (toString ("could not parse repo manifest file: " <> xmlErrorPretty err))

  describe "repo manifest buildGraph" $ do
    describe "for a sane manifest" $
      it "builds a graph properly" $
        case parseXML basicManifest of
          Right manifest -> do
            let projects = case validateProjects manifest of
                  Nothing -> []
                  (Just ps) -> ps
            let graph = buildGraph projects
            let vps = validateProject manifest <$> manifestProjects manifest
            vps `shouldMatchList` [Just validatedProjectOne, Just validatedProjectTwo, Just validatedProjectThree, Just validatedProjectFour, Just validatedProjectFive]
            expectDirect [dependencyOne, dependencyTwo, dependencyThree, dependencyFour, dependencyFive] graph
          Left err -> expectationFailure (toString ("could not parse repo manifest file: " <> xmlErrorPretty err))

    describe "for a manifest with no default remote" $
      it "returns nothing for validateProject on a project with no remote attr" $
        case parseXML noDefaultRemoteManifest of
          Right manifest -> do
            let vps = validateProject manifest <$> manifestProjects manifest
            vps `shouldMatchList` [Just validatedProjectOne, Just validatedProjectTwo, Just validatedProjectThree, Just validatedProjectFour, Nothing]
          Left err -> expectationFailure (toString ("could not parse repo manifest file: " <> xmlErrorPretty err))

    describe "for a manifest with no default revision" $
      it "finds the projects with remotes specified" $
        case parseXML noDefaultRevisionManifest of
          Right manifest -> do
            let vps = validateProject manifest <$> manifestProjects manifest
            vps `shouldMatchList` [Nothing, Just validatedProjectTwo, Just validatedProjectThree, Just validatedProjectFour, Nothing]
          Left err -> expectationFailure $ toString $ "could not parse repo manifest file: " <> xmlErrorPretty err

    describe "for a manifest with an include tag" $ do
      projectsForManifestWithIncludes <- runIt $ nestedValidatedProjects (currentDir </> $(mkRelDir "test/Googlesource/testdata")) (currentDir </> $(mkRelFile "test/Googlesource/testdata/manifest-with-include.xml"))
      it "reads both files and gets the dependencies from the included file" $
        assertOnSuccess projectsForManifestWithIncludes (\_ projects -> projects `shouldMatchList` validatedProjectList)

    describe "for a manifest with a relative remote and no include" $ do
      projectsForManifestWithRelativeRemoteNoInclude <- runIt $ nestedValidatedProjects (currentDir </> $(mkRelDir "test/Googlesource/testdata/manifest-with-relative-remote-url")) (currentDir </> $(mkRelFile "test/Googlesource/testdata/manifest-with-relative-remote-url/manifest-without-include.xml"))
      it "gets the remote from the git config" $
        assertOnSuccess projectsForManifestWithRelativeRemoteNoInclude (\_ projects -> projects `shouldMatchList` validatedProjectList)

    describe "for a manifest with a relative remote and an include" $ do
      projectsForManifestWithRelativeRemoteWithInclude <- runIt $ nestedValidatedProjects (currentDir </> $(mkRelDir "test/Googlesource/testdata/manifest-with-relative-remote-url")) (currentDir </> $(mkRelFile "test/Googlesource/testdata/manifest-with-relative-remote-url/manifest-without-include.xml"))
      it "gets the remote from the git config" $
        assertOnSuccess projectsForManifestWithRelativeRemoteWithInclude (\_ projects -> projects `shouldMatchList` validatedProjectList)
