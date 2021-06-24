{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module App.Fossa.ManualDeps (
  CustomDependency (..),
  ReferencedDependency (..),
  VendoredDependency (..),
  ManualDependencies (..),
  FoundDepsFile (..),
  analyzeFossaDepsFile,
) where

import Control.Effect.Diagnostics (Diagnostics, context, fatalText)
import Control.Monad (when)
import Data.Aeson (
  FromJSON (parseJSON),
  withObject,
  (.!=),
  (.:),
  (.:?),
 )

import App.Fossa.ArchiveUploader
import Control.Effect.Lift
import Data.Aeson.Extra
import Data.Aeson.Types (Parser)
import Data.Functor.Extra ((<$$>))
import Data.List.NonEmpty qualified as NE
import Data.String.Conversion (toText)
import Data.Text (Text, unpack)
import DepTypes (DepType (..))
import Effect.ReadFS (ReadFS, doesFileExist, readContentsYaml, readContentsJson)
import Fossa.API.Types
import Path
import Srclib.Converter (depTypeToFetcher)
import Srclib.Types (AdditionalDepData (..), Locator (..), SourceUnit (..), SourceUnitBuild (..), SourceUnitDependency (SourceUnitDependency), SourceUserDefDep (..))

data FoundDepsFile
  = ManualYaml (Path Abs File)
  | ManualJSON (Path Abs File)

analyzeFossaDepsFile :: (Has Diagnostics sig m, Has ReadFS sig m, Has (Lift IO) sig m) => Path Abs Dir -> Maybe ApiOpts -> m (Maybe SourceUnit)
analyzeFossaDepsFile root maybeApiOpts = do
  maybeDepsFile <- findFossaDepsFile root
  case maybeDepsFile of
    Nothing -> pure Nothing
    Just depsFile -> do
      manualDeps <- context "Reading fossa-deps file" $ readFoundDeps depsFile
      context "Converting fossa-deps to partial API payload" $ Just <$> toSourceUnit root manualDeps maybeApiOpts

readFoundDeps :: (Has Diagnostics sig m, Has ReadFS sig m) => FoundDepsFile -> m ManualDependencies
readFoundDeps (ManualJSON path) = readContentsJson path
readFoundDeps (ManualYaml path) = readContentsYaml path

findFossaDepsFile :: (Has Diagnostics sig m, Has ReadFS sig m) => Path Abs Dir -> m (Maybe FoundDepsFile)
findFossaDepsFile root = do
  let ymlFile = root </> $(mkRelFile "fossa-deps.yml")
      yamlFile = root </> $(mkRelFile "fossa-deps.yaml")
      jsonFile = root </> $(mkRelFile "fossa-deps.json")
      multipleFound = fatalText "Found multiple fossa-deps files.  Only one of ('.json', '.yml', and '.yaml') extensions are allowed"
  ymlExists <- doesFileExist ymlFile
  yamlExists <- doesFileExist yamlFile
  jsonExists <- doesFileExist jsonFile
  case (ymlExists, yamlExists, jsonExists) of
    -- Allow 0 or 1 files, not multiple
    (True, True, _) -> multipleFound
    (_, True, True) -> multipleFound
    (True, _, True) -> multipleFound
    (True, _, _) -> pure $ Just $ ManualYaml ymlFile
    (_, True, _) -> pure $ Just $ ManualYaml yamlFile
    (_, _, True) -> pure $ Just $ ManualJSON jsonFile
    (False, False, False) -> pure Nothing

toSourceUnit :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs Dir -> ManualDependencies -> Maybe ApiOpts -> m SourceUnit
toSourceUnit root manualDeps@ManualDependencies{..} maybeApiOpts = do
  -- If the file exists and we have no dependencies to report, that's a failure.
  when (hasNoDeps manualDeps) $ fatalText "No dependencies found in fossa-deps file"
  archiveLocators <- case maybeApiOpts of
    Nothing -> pure $ archiveNoUploadSourceUnit vendoredDependencies
    Just apiOpts -> archiveUploadSourceUnit root apiOpts vendoredDependencies

  let renderedPath = toText root
      referenceLocators = refToLocator <$> referencedDependencies
      additional = toAdditionalData <$> NE.nonEmpty customDependencies
      build = toBuildData <$> NE.nonEmpty (referenceLocators <> archiveLocators)
  pure $
    SourceUnit
      { sourceUnitName = renderedPath
      , sourceUnitManifest = renderedPath
      , sourceUnitType = "user-specific-yaml"
      , sourceUnitBuild = build
      , additionalData = additional
      }

toBuildData :: NE.NonEmpty Locator -> SourceUnitBuild
toBuildData locators =
  SourceUnitBuild
    { buildArtifact = "default"
    , buildSucceeded = True
    , buildImports = NE.toList locators
    , buildDependencies = map addEmptyDep $ NE.toList locators
    }

refToLocator :: ReferencedDependency -> Locator
refToLocator ReferencedDependency{..} =
  Locator
    { locatorFetcher = depTypeToFetcher locDepType
    , locatorProject = locDepName
    , locatorRevision = locDepVersion
    }

addEmptyDep :: Locator -> SourceUnitDependency
addEmptyDep loc = SourceUnitDependency loc []

toAdditionalData :: NE.NonEmpty CustomDependency -> AdditionalDepData
toAdditionalData deps = AdditionalDepData{userDefinedDeps = map toSrc $ NE.toList deps}
  where
    toSrc CustomDependency{..} =
      SourceUserDefDep
        { srcUserDepName = customName
        , srcUserDepVersion = customVersion
        , srcUserDepLicense = customLicense
        , srcUserDepDescription = customDescription
        , srcUserDepUrl = customUrl
        }

hasNoDeps :: ManualDependencies -> Bool
hasNoDeps ManualDependencies{..} = null referencedDependencies && null customDependencies && null vendoredDependencies

data ManualDependencies = ManualDependencies
  { referencedDependencies :: [ReferencedDependency]
  , customDependencies :: [CustomDependency]
  , vendoredDependencies :: [VendoredDependency]
  }
  deriving (Eq, Ord, Show)

data ReferencedDependency = ReferencedDependency
  { locDepName :: Text
  , locDepType :: DepType
  , locDepVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data CustomDependency = CustomDependency
  { customName :: Text
  , customVersion :: Text
  , customLicense :: Text
  , customDescription :: Maybe Text
  , customUrl :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON ManualDependencies where
  parseJSON = withObject "ManualDependencies" $ \obj ->
    ManualDependencies <$ (obj .:? "version" >>= isMissingOr1)
      <*> (obj .:? "referenced-dependencies" .!= [])
      <*> (obj .:? "custom-dependencies" .!= [])
      <*> (obj .:? "vendored-dependencies" .!= [])
    where
      isMissingOr1 :: Maybe Int -> Parser ()
      isMissingOr1 (Just x) | x /= 1 = fail $ "Invalid fossa-deps version: " <> show x
      isMissingOr1 _ = pure ()

depTypeParser :: Text -> Parser DepType
depTypeParser text = case depTypeFromText text of
  Just t -> pure t
  Nothing -> fail $ "dep type: " <> unpack text <> " not supported"

instance FromJSON ReferencedDependency where
  parseJSON = withObject "ReferencedDependency" $ \obj ->
    ReferencedDependency <$> obj .: "name"
      <*> (obj .: "type" >>= depTypeParser)
      <*> (unTextLike <$$> obj .:? "version")
      <* forbidMembers "referenced dependencies" ["license", "description", "url", "path"] obj

instance FromJSON CustomDependency where
  parseJSON = withObject "CustomDependency" $ \obj ->
    CustomDependency <$> obj .: "name"
      <*> (unTextLike <$> obj .: "version")
      <*> obj .: "license"
      <*> obj .:? "description"
      <*> obj .:? "url"
      <* forbidMembers "custom dependencies" ["type", "path"] obj

-- Parse supported dependency types into their respective type or return Nothing.
depTypeFromText :: Text -> Maybe DepType
depTypeFromText text = case text of
  "cargo" -> Just CargoType
  "carthage" -> Just CarthageType
  "composer" -> Just ComposerType
  "gem" -> Just GemType
  "git" -> Just GitType
  "go" -> Just GoType
  "hackage" -> Just HackageType
  "hex" -> Just HexType
  "maven" -> Just MavenType
  "npm" -> Just NodeJSType
  "nuget" -> Just NuGetType
  "pypi" -> Just PipType
  "cocoapods" -> Just PodType
  "url" -> Just URLType
  _ -> Nothing -- unsupported dep, need to respond with an error and skip this dependency
  -- rpm is an unsupported type. This is because we currently have 2 RPM fetchers
  -- and we should wait for a need to determine which one to use for manually
  -- specified dependencies.
