{-# language TemplateHaskell #-}

module Googlesource.RepoManifestTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GraphUtil
import DepTypes
import Parse.XML
import Strategy.Googlesource.RepoManifest
import Test.Tasty.Hspec
import Effect.ReadFS
import Control.Carrier.Error.Either
import Control.Carrier.Fail.Either

-- <remote name="aosp" fetch="https://android.googlesource.com" />
remoteOne :: ManifestRemote
remoteOne = ManifestRemote { remoteName     = "aosp"
                           , remoteFetch    = "https://android.googlesource.com"
                           , remoteRevision = Nothing
                           }

-- <remote fetch="https://android.othersource.com" name="othersource" revision="google/android-6.0.1_r74" />
remoteTwo :: ManifestRemote
remoteTwo = ManifestRemote { remoteName     = "othersource"
                           , remoteFetch    = "https://android.othersource.com"
                           , remoteRevision = Just "google/android-6.0.1_r74"
                           }

basicRemoteList :: [ManifestRemote]
basicRemoteList = [remoteOne, remoteTwo]

-- <default revision="refs/tags/android-10.0.0_r29"
--          remote="aosp"
--          sync-j="4" />
basicDefault :: ManifestDefault
basicDefault = ManifestDefault { defaultRemote   = Just "aosp"
                               , defaultRevision = Just "refs/tags/android-10.0.0_r29"
                               }

-- <project path="art" name="platform/art" groups="pdk" />
projectOne :: ManifestProject
projectOne = ManifestProject { projectName     = "platform/art"
                             , projectPath     = Just "art"
                             , projectRevision = Nothing
                             , projectRemote   = Just "aosp"
                             }
-- <project path="bionic" name="platform/bionic" groups="pdk" revision="57b7d1574276f5e7f895c884df29f45859da74b6" />
projectTwo :: ManifestProject
projectTwo = ManifestProject { projectName     = "platform/bionic"
                             , projectPath     = Just "bionic"
                             , projectRevision = Just "57b7d1574276f5e7f895c884df29f45859da74b6"
                             , projectRemote   = Nothing
                             }
-- <project path="bionic" name="platform/bionic" groups="pdk" revision="57b7d1574276f5e7f895c884df29f45859da74b6" remote="aosp" />
projectTwoWithRemote :: ManifestProject
projectTwoWithRemote = ManifestProject { projectName     = "platform/bionic"
                             , projectPath     = Just "bionic"
                             , projectRevision = Just "57b7d1574276f5e7f895c884df29f45859da74b6"
                             , projectRemote   = Just "aosp"
                             }
-- <project path="bootable/recovery" name="platform/bootable/recovery" groups="pdk" remote="othersource" />
projectThree :: ManifestProject
projectThree = ManifestProject { projectName     = "platform/bootable/recovery"
                               , projectPath     = Just "bootable/recovery"
                               , projectRevision = Nothing
                               , projectRemote   = Just "othersource"
                               }
-- <project path="cts" name="platform/cts" groups="cts,pdk-cw-fs,pdk-fs" remote="othersource" revision="1111"/>
projectFour :: ManifestProject
projectFour = ManifestProject { projectName    = "platform/cts"
                             , projectPath     = Just "cts"
                             , projectRevision = Just "1111"
                             , projectRemote   = Just "othersource"
                             }
-- <project path="dalvik" name="platform/dalvik" groups="pdk-cw-fs,pdk-fs" />
projectFive :: ManifestProject
projectFive = ManifestProject { projectName    = "platform/dalvik"
                             , projectPath     = Just "dalvik"
                             , projectRevision = Nothing
                             , projectRemote   = Nothing
                             }

basicProjectList :: [ManifestProject]
basicProjectList = [projectOne, projectTwo, projectThree, projectFour, projectFive]

dependencyOne :: Dependency
dependencyOne = Dependency { dependencyType = GooglesourceType
                           , dependencyName = "platform/art"
                           , dependencyVersion = Just (CEq "refs/tags/android-10.0.0_r29")
                           , dependencyLocations = ["https://android.googlesource.com/platform/art"]
                           , dependencyTags = M.empty
                           , dependencyEnvironments = [EnvProduction]
                           }

dependencyTwo :: Dependency
dependencyTwo = Dependency { dependencyType = GooglesourceType
                           , dependencyName = "platform/bionic"
                           , dependencyVersion = Just (CEq "57b7d1574276f5e7f895c884df29f45859da74b6")
                           , dependencyLocations = ["https://android.googlesource.com/platform/bionic"]
                           , dependencyTags = M.empty
                           , dependencyEnvironments = [EnvProduction]
                           }

dependencyThree :: Dependency
dependencyThree = Dependency { dependencyType = GooglesourceType
                             , dependencyName = "platform/bootable/recovery"
                             , dependencyVersion = Just (CEq "google/android-6.0.1_r74")
                             , dependencyLocations = ["https://android.othersource.com/platform/bootable/recovery"]
                             , dependencyTags = M.empty
                             , dependencyEnvironments = [EnvProduction]
                             }

dependencyFour :: Dependency
dependencyFour = Dependency { dependencyType = GooglesourceType
                            , dependencyName = "platform/cts"
                            , dependencyVersion = Just (CEq "1111")
                            , dependencyLocations = ["https://android.othersource.com/platform/cts"]
                            , dependencyTags = M.empty
                            , dependencyEnvironments = [EnvProduction]
                            }

dependencyFive :: Dependency
dependencyFive = Dependency { dependencyType = GooglesourceType
                            , dependencyName = "platform/dalvik"
                            , dependencyVersion = Just (CEq "refs/tags/android-10.0.0_r29")
                            , dependencyLocations = ["https://android.googlesource.com/platform/dalvik"]
                            , dependencyTags = M.empty
                            , dependencyEnvironments = [EnvProduction]
                            }

validatedProjectOne :: ValidatedProject
validatedProjectOne = ValidatedProject { validatedProjectName = "platform/art"
                                       , validatedProjectPath = "art"
                                       , validatedProjectUrl = "https://android.googlesource.com/platform/art"
                                       , validatedProjectRevision = "refs/tags/android-10.0.0_r29"
                                       }

validatedProjectTwo :: ValidatedProject
validatedProjectTwo = ValidatedProject { validatedProjectName = "platform/bionic"
                                       , validatedProjectPath = "bionic"
                                       , validatedProjectUrl = "https://android.googlesource.com/platform/bionic"
                                       , validatedProjectRevision = "57b7d1574276f5e7f895c884df29f45859da74b6"
                                       }

validatedProjectThree :: ValidatedProject
validatedProjectThree = ValidatedProject { validatedProjectName = "platform/bootable/recovery"
                                       , validatedProjectPath = "bootable/recovery"
                                       , validatedProjectUrl = "https://android.othersource.com/platform/bootable/recovery"
                                       , validatedProjectRevision = "google/android-6.0.1_r74"
                                       }

validatedProjectFour :: ValidatedProject
validatedProjectFour = ValidatedProject { validatedProjectName = "platform/cts"
                                       , validatedProjectPath = "cts"
                                       , validatedProjectUrl = "https://android.othersource.com/platform/cts"
                                       , validatedProjectRevision = "1111"
                                       }

validatedProjectFive :: ValidatedProject
validatedProjectFive = ValidatedProject { validatedProjectName = "platform/dalvik"
                                       , validatedProjectPath = "dalvik"
                                       , validatedProjectUrl = "https://android.googlesource.com/platform/dalvik"
                                       , validatedProjectRevision = "refs/tags/android-10.0.0_r29"
                                       }

spec_analyze :: Spec
spec_analyze = do
  basicManifest <- runIO (TIO.readFile "test/Googlesource/testdata/manifest.xml")
  noDefaultRemoteManifest <- runIO (TIO.readFile "test/Googlesource/testdata/manifest-no-default-remote.xml")
  noDefaultRevisionManifest <- runIO (TIO.readFile "test/Googlesource/testdata/manifest-no-default-revision.xml")
  projectsForManifestWithIncludes <- runIO $ runFail $ runError @ReadFSErr $ runReadFSIO $ nestedValidatedProjects $(mkRelFile "test/Googlesource/testdata/manifest-with-include.xml")

  describe "repo manifest analyzer" $ do
    describe "for a sane manifest" $ do
      it "reads a file and constructs a dependency list" $ do
        case parseXML basicManifest of
          Right manifest -> do
            (manifestProjects manifest) `shouldMatchList` basicProjectList
            (manifestDefault manifest) `shouldBe` Just basicDefault
            (manifestRemotes manifest) `shouldMatchList` basicRemoteList
          Left err -> expectationFailure (T.unpack ("could not parse repo manifest file: " <> xmlErrorPretty err))

    describe "for a manifest with no default remote" $ do
      it "reads a file and constructs a dependency list" $ do
        case parseXML noDefaultRemoteManifest of
          Right manifest -> do
            (manifestProjects manifest) `shouldMatchList` [projectOne, projectTwoWithRemote, projectThree, projectFour, projectFive]
            (manifestRemotes manifest) `shouldMatchList` basicRemoteList
          Left err -> expectationFailure (T.unpack ("could not parse repo manifest file: " <> xmlErrorPretty err))

    describe "for a manifest with no default revision" $ do
      it "reads a file and constructs a dependency list" $ do
        case parseXML noDefaultRevisionManifest of
          Right manifest -> do
            (manifestProjects manifest) `shouldMatchList` basicProjectList
            (manifestRemotes manifest) `shouldMatchList` basicRemoteList
          Left err -> expectationFailure (T.unpack ("could not parse repo manifest file: " <> xmlErrorPretty err))

  describe "repo manifest buildGraph" $ do
    describe "for a sane manifest" $ do
      it "builds a graph properly" $ do
        case parseXML basicManifest of
          Right manifest -> do
            let projects = case validateProjects manifest of
                        Nothing -> []
                        (Just ps) -> ps
            let graph = buildGraph projects
            let vps = validateProject manifest <$> manifestProjects manifest
            vps `shouldMatchList` [Just validatedProjectOne, Just validatedProjectTwo, Just validatedProjectThree, Just validatedProjectFour, Just validatedProjectFive]
            expectDirect [dependencyOne, dependencyTwo, dependencyThree, dependencyFour, dependencyFive] graph
          Left err -> expectationFailure (T.unpack ("could not parse repo manifest file: " <> xmlErrorPretty err))

    describe "for a manifest with no default remote" $ do
      it "returns nothing for validateProject on a project with no remote attr" $ do
        case parseXML noDefaultRemoteManifest of
          Right manifest -> do
            let vps = validateProject manifest <$> manifestProjects manifest
            vps `shouldMatchList` [Just validatedProjectOne, Just validatedProjectTwo, Just validatedProjectThree, Just validatedProjectFour, Nothing]

          Left err -> expectationFailure (T.unpack ("could not parse repo manifest file: " <> xmlErrorPretty err))

    describe "for a manifest with no default revision" $ do
      it "finds the projects with remotes specified" $ do
        case parseXML noDefaultRevisionManifest of
          Right manifest -> do
            let vps = validateProject manifest <$> manifestProjects manifest
            vps `shouldMatchList` [Nothing, Just validatedProjectTwo, Just validatedProjectThree, Just validatedProjectFour, Nothing]
          Left err -> expectationFailure (T.unpack ("could not parse repo manifest file: " <> xmlErrorPretty err))

    describe "for a manifest with an include tag" $ do
      it "reads both files and gets the dependencies from the included file" $ do
        case projectsForManifestWithIncludes of
          Left _ -> expectationFailure("could not parse nested manifest")
          Right (Left _) -> expectationFailure("could not parse nested manifest, second level")
          Right (Right ps) -> ps `shouldMatchList` [validatedProjectOne, validatedProjectTwo, validatedProjectThree, validatedProjectFour, validatedProjectFive]
