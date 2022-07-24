{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.ManualDeps (
  ReferencedDependency (..),
  CustomDependency (..),
  RemoteDependency (..),
  DependencyMetadata (..),
  VendoredDependency (..),
  ManualDependencies (..),
  FoundDepsFile (..),
  analyzeFossaDepsFile,
  findFossaDepsFile,
  readFoundDeps,
  getScanCfg,
) where

import App.Fossa.ArchiveUploader (archiveUploadSourceUnit)
import App.Fossa.Config.Analyze (
  VendoredDependencyOptions (..),
 )
import App.Fossa.LicenseScanner (licenseScanSourceUnit)
import App.Fossa.VendoredDependency (
  VendoredDependency (..),
  VendoredDependencyScanMode (..),
  arcToLocator,
  forceVendoredToArchive,
 )
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Effect.Diagnostics (Diagnostics, context, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
import Control.Effect.Lift (Has, Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Control.Monad (when)
import Data.Aeson (
  FromJSON (parseJSON),
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson.Extra (TextLike (unTextLike), forbidMembers)
import Data.Aeson.Types (Parser)
import Data.Functor.Extra ((<$$>))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import DepTypes (DepType (..))
import Effect.Exec (Exec)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS, doesFileExist, readContentsJson, readContentsYaml)
import Fossa.API.Types (ApiOpts, Organization (..))
import Path (Abs, Dir, File, Path, mkRelFile, (</>))
import Path.Extra (tryMakeRelative)
import Srclib.Converter (depTypeToFetcher)
import Srclib.Types (AdditionalDepData (..), Locator (..), SourceRemoteDep (..), SourceUnit (..), SourceUnitBuild (..), SourceUnitDependency (SourceUnitDependency), SourceUserDefDep (..))
import Types (ArchiveUploadType (..), GraphBreadth (..))

data FoundDepsFile
  = ManualYaml (Path Abs File)
  | ManualJSON (Path Abs File)

analyzeFossaDepsFile ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  VendoredDependencyOptions ->
  m (Maybe SourceUnit)
analyzeFossaDepsFile root maybeApiOpts vendoredDepsOptions = do
  maybeDepsFile <- findFossaDepsFile root
  case maybeDepsFile of
    Nothing -> pure Nothing
    Just depsFile -> do
      manualDeps <- context "Reading fossa-deps file" $ readFoundDeps depsFile
      context "Converting fossa-deps to partial API payload" $ Just <$> toSourceUnit root depsFile manualDeps maybeApiOpts vendoredDepsOptions

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

toSourceUnit ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  FoundDepsFile ->
  ManualDependencies ->
  Maybe ApiOpts ->
  VendoredDependencyOptions ->
  m SourceUnit
toSourceUnit root depsFile manualDeps@ManualDependencies{..} maybeApiOpts vendoredDepsOptions = do
  -- If the file exists and we have no dependencies to report, that's a failure.
  when (hasNoDeps manualDeps) $ fatalText "No dependencies found in fossa-deps file"

  archiveLocators <- case (maybeApiOpts, NE.nonEmpty vendoredDependencies) of
    (Just apiOpts, Just vdeps) -> NE.toList <$> runFossaApiClient apiOpts (scanAndUpload root vdeps vendoredDepsOptions)
    (Nothing, Just vdeps) -> pure $ noSourceUnits $ NE.toList vdeps
    -- Don't do anything if there are no vendored deps.
    (_, Nothing) -> pure []

  let renderedPath = toText root
      referenceLocators = refToLocator <$> referencedDependencies
      additional = toAdditionalData (NE.nonEmpty customDependencies) (NE.nonEmpty remoteDependencies)
      build = toBuildData <$> NE.nonEmpty (referenceLocators <> archiveLocators)
      originPath = case depsFile of
        (ManualJSON path) -> tryMakeRelative root path
        (ManualYaml path) -> tryMakeRelative root path
  pure $
    SourceUnit
      { sourceUnitName = renderedPath
      , sourceUnitManifest = renderedPath
      , sourceUnitType = "user-specific-yaml"
      , sourceUnitBuild = build
      , sourceUnitGraphBreadth = Complete
      , sourceUnitOriginPaths = [originPath]
      , additionalData = additional
      }

-- | Run either archive upload or native license scan.
scanAndUpload ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has FossaApiClient sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  NonEmpty VendoredDependency ->
  VendoredDependencyOptions ->
  m (NonEmpty Locator)
scanAndUpload root vdeps vendoredDepsOptions = do
  org <- getOrganization
  (archiveOrCLI, vendoredDependencyScanMode) <- getScanCfg org vendoredDepsOptions
  let scanner = case archiveOrCLI of
        ArchiveUpload -> archiveUploadSourceUnit
        CLILicenseScan -> licenseScanSourceUnit vendoredDependencyScanMode
  scanner root vdeps

getScanCfg :: (Has Diagnostics sig m) => Organization -> VendoredDependencyOptions -> m (ArchiveUploadType, VendoredDependencyScanMode)
getScanCfg Organization{..} VendoredDependencyOptions{..} = do
  archiveOrCLI <-
    case (orgCoreSupportsLocalLicenseScan, orgDefaultVendoredDependencyScanType, licenseScanMethod) of
      (False, _, Just CLILicenseScan) -> fatalText "You provided the --force-vendored-dependencies-license-scan flag but the FOSSA server does not support CLI-side license scans"
      (False, _, _) -> pure ArchiveUpload
      (True, _, Just ArchiveUpload) -> pure ArchiveUpload
      (True, _, Just CLILicenseScan) -> pure CLILicenseScan
      (True, orgDefault, Nothing) -> pure orgDefault
  let vendoredDependencyScanMode =
        case (orgSupportsAnalyzedRevisionsQuery, forceRescans) of
          -- The --force-vendored-dependency-rescans flag should win so that we can force rebuilds even if Core does not support skipping
          (_, True) -> SkippingDisabledViaFlag
          (False, False) -> SkippingNotSupported
          (True, False) -> SkipPreviouslyScanned
  pure (archiveOrCLI, vendoredDependencyScanMode)

-- | Used when users run `fossa analyze -o` and do not upload their source units.
noSourceUnits :: [VendoredDependency] -> [Locator]
noSourceUnits = map (arcToLocator . forceVendoredToArchive)

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

toAdditionalData :: Maybe (NE.NonEmpty CustomDependency) -> Maybe (NE.NonEmpty RemoteDependency) -> Maybe AdditionalDepData
toAdditionalData customDeps remoteDeps =
  Just
    AdditionalDepData
      { userDefinedDeps = map toCustom . NE.toList <$> customDeps
      , remoteDeps = map toUrl . NE.toList <$> remoteDeps
      }
  where
    toCustom CustomDependency{..} =
      SourceUserDefDep
        { srcUserDepName = customName
        , srcUserDepVersion = customVersion
        , srcUserDepLicense = customLicense
        , srcUserDepDescription = customMetadata >>= depDescription
        , srcUserDepHomepage = customMetadata >>= depHomepage
        , srcUserDepOrigin = Nothing
        }
    toUrl RemoteDependency{..} =
      SourceRemoteDep
        { srcRemoteDepName = remoteName
        , srcRemoteDepVersion = remoteVersion
        , srcRemoteDepUrl = remoteUrl
        , srcRemoteDepDescription = remoteMetadata >>= depDescription
        , srcRemoteDepHomepage = remoteMetadata >>= depHomepage
        }

hasNoDeps :: ManualDependencies -> Bool
hasNoDeps ManualDependencies{..} = null referencedDependencies && null customDependencies && null vendoredDependencies && null remoteDependencies

-- TODO: Change these to Maybe NonEmpty
data ManualDependencies = ManualDependencies
  { referencedDependencies :: [ReferencedDependency]
  , customDependencies :: [CustomDependency]
  , vendoredDependencies :: [VendoredDependency]
  , remoteDependencies :: [RemoteDependency]
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
  , customMetadata :: Maybe DependencyMetadata
  }
  deriving (Eq, Ord, Show)

data RemoteDependency = RemoteDependency
  { remoteName :: Text
  , remoteVersion :: Text
  , remoteUrl :: Text
  , remoteMetadata :: Maybe DependencyMetadata
  }
  deriving (Eq, Ord, Show)

data DependencyMetadata = DependencyMetadata
  { depDescription :: Maybe Text
  , depHomepage :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON ManualDependencies where
  parseJSON = withObject "ManualDependencies" $ \obj ->
    ManualDependencies
      <$ (obj .:? "version" >>= isMissingOr1)
      <*> (obj .:? "referenced-dependencies" .!= [])
      <*> (obj .:? "custom-dependencies" .!= [])
      <*> (obj .:? "vendored-dependencies" .!= [])
      <*> (obj .:? "remote-dependencies" .!= [])
    where
      isMissingOr1 :: Maybe Int -> Parser ()
      isMissingOr1 (Just x) | x /= 1 = fail $ "Invalid fossa-deps version: " <> show x
      isMissingOr1 _ = pure ()

depTypeParser :: Text -> Parser DepType
depTypeParser text = case depTypeFromText text of
  Just t -> pure t
  Nothing -> fail $ "dep type: " <> toString text <> " not supported"

instance FromJSON ReferencedDependency where
  parseJSON = withObject "ReferencedDependency" $ \obj ->
    ReferencedDependency
      <$> obj .: "name"
      <*> (obj .: "type" >>= depTypeParser)
      <*> (unTextLike <$$> obj .:? "version")
      <* forbidMembers "referenced dependencies" ["license", "description", "url", "path"] obj

instance FromJSON CustomDependency where
  parseJSON = withObject "CustomDependency" $ \obj ->
    CustomDependency
      <$> obj .: "name"
      <*> (unTextLike <$> obj .: "version")
      <*> obj .: "license"
      <*> obj .:? "metadata"
      <* forbidMembers "custom dependencies" ["type", "path", "url"] obj

instance FromJSON RemoteDependency where
  parseJSON = withObject "RemoteDependency" $ \obj ->
    RemoteDependency
      <$> obj .: "name"
      <*> (unTextLike <$> obj .: "version")
      <*> obj .: "url"
      <*> obj .:? "metadata"
      <* forbidMembers "remote dependencies" ["license", "path", "type"] obj

-- Dependency "metadata" section for both Remote and Custom Dependencies
instance FromJSON DependencyMetadata where
  parseJSON = withObject "metadata" $ \obj ->
    DependencyMetadata
      <$> obj .:? "description"
      <*> obj .:? "homepage"
      <* forbidMembers "metadata" ["url"] obj

-- Parse supported dependency types into their respective type or return Nothing.
depTypeFromText :: Text -> Maybe DepType
depTypeFromText text = case text of
  "bower" -> Just BowerType
  "cargo" -> Just CargoType
  "carthage" -> Just CarthageType
  "composer" -> Just ComposerType
  "cpan" -> Just CpanType
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
  "swift" -> Just SwiftType
  _ -> Nothing -- unsupported dep, need to respond with an error and skip this dependency
  -- rpm is an unsupported type. This is because we currently have 2 RPM fetchers
  -- and we should wait for a need to determine which one to use for manually
  -- specified dependencies.
