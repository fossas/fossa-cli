{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.ManualDeps (
  ReferencedDependency (..),
  ManagedReferenceDependency (..),
  LinuxReferenceDependency (..),
  CustomDependency (..),
  RemoteDependency (..),
  DependencyMetadata (..),
  VendoredDependency (..),
  ManualDependencies (..),
  FoundDepsFile (..),
  analyzeFossaDepsFile,
  findAndReadFossaDepsFile,
  findFossaDepsFile,
  readFoundDeps,
  getScanCfg,
)
where

import App.Fossa.ArchiveUploader (archiveUploadSourceUnit)
import App.Fossa.Config.Analyze (
  VendoredDependencyOptions (..),
 )
import App.Fossa.Config.Common (validateFile)
import App.Fossa.LicenseScanner (licenseScanSourceUnit)
import App.Fossa.ManualDepsTypes (
  ReferencedDependency (..),
  ManagedReferenceDependency (..),
  LinuxReferenceDependency (..),
  CustomDependency (..),
  RemoteDependency (..),
  DependencyMetadata (..),
  VendoredDependency (..),
  ManualDependencies (..),
  FoundDepsFile (..),
  )
import App.Fossa.VendoredDependency (
  VendoredDependencyScanMode (..),
  arcToLocator,
  forceVendoredToArchive,
  vendoredDependencyScanModeToDependencyRebuild,
 )
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (Diagnostics, context, errCtx, errHelp, fatal, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
import Control.Effect.Lift (Has, Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Control.Monad (when)
import Data.Error (SourceLocation, createEmptyBlock, createErrataWithHeaderOnly, getSourceLocation)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, isJust)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Effect.Exec (Exec)
import Effect.Logger (Logger, indent, pretty, renderIt, vsep)
import Effect.ReadFS (ReadFS, doesFileExist, readContentsJson, readContentsYaml)
import Errata (Errata (..), errataSimple)
import Fossa.API.Types (ApiOpts, Organization (..), orgFileUpload)
import Path (Abs, Dir, Path, mkRelFile, (</>))
import Path.Extra (tryMakeRelative)
import Srclib.Converter (depTypeToFetcher)
import Srclib.Types (AdditionalDepData (..), Locator (..), SourceRemoteDep (..), SourceUnit (..), SourceUnitBuild (..), SourceUnitDependency (SourceUnitDependency), SourceUserDefDep (..), someBaseToOriginPath)
import System.FilePath (takeExtension)
import Types (ArchiveUploadType (..), GraphBreadth (..))


analyzeFossaDepsFile ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Debug sig m
  , Has Exec sig m
  ) =>
  Path Abs Dir ->
  Maybe FilePath ->
  Maybe ApiOpts ->
  VendoredDependencyOptions ->
  m (Maybe SourceUnit)
analyzeFossaDepsFile root maybeCustomFossaDepsPath maybeApiOpts vendoredDepsOptions = do
  maybeDepsFile <-
    case maybeCustomFossaDepsPath of
      Nothing -> findFossaDepsFile root
      Just filePath -> retrieveCustomFossaDepsFile filePath
  case maybeDepsFile of
    Nothing -> pure Nothing
    Just depsFile -> do
      manualDeps <- context "Reading fossa-deps file" $ readFoundDeps depsFile
      if hasNoDeps manualDeps
        then pure Nothing
        else
          context "Converting fossa-deps to partial API payload" $
            Just <$> toSourceUnit root depsFile manualDeps maybeApiOpts vendoredDepsOptions

retrieveCustomFossaDepsFile ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  FilePath ->
  m (Maybe FoundDepsFile)
retrieveCustomFossaDepsFile fossaDepsPath = do
  let extension = takeExtension fossaDepsPath
  file <- validateFile fossaDepsPath

  case extension of
    ".yml" -> pure $ Just $ ManualYaml file
    ".yaml" -> pure $ Just $ ManualYaml file
    ".json" -> pure $ Just $ ManualJSON file
    _ -> fatalText $ "Expected <name-of-file>.{yml|yaml|json} but received: " <> toText fossaDepsPath

findAndReadFossaDepsFile ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  m (Maybe ManualDependencies)
findAndReadFossaDepsFile root = do
  maybeDepsFile <- findFossaDepsFile root
  case maybeDepsFile of
    Nothing -> pure Nothing
    Just depsFile -> do
      manualDeps <- readFoundDeps depsFile
      pure $ Just manualDeps

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
  , Has Debug sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  FoundDepsFile ->
  ManualDependencies ->
  Maybe ApiOpts ->
  VendoredDependencyOptions ->
  m SourceUnit
toSourceUnit root depsFile manualDeps@ManualDependencies{..} maybeApiOpts vendoredDepsOptions = do
  when (hasNoDeps manualDeps) $ fatalText "No dependencies found in fossa-deps file"

  archiveLocators <- case (maybeApiOpts, NE.nonEmpty vendoredDependencies) of
    (Just apiOpts, Just vdeps) -> NE.toList <$> runFossaApiClient apiOpts (scanAndUpload root vdeps vendoredDepsOptions)
    (Nothing, Just vdeps) -> pure $ noSourceUnits $ NE.toList vdeps
    -- Don't do anything if there are no vendored deps.
    (_, Nothing) -> pure []

  -- Some manual deps, such as remote dependencies in source unit cannot be
  -- validated without endpoint interactions.
  rdeps <- case maybeApiOpts of
    Just apiOpts -> runFossaApiClient apiOpts $ do
      org <- getOrganization
      traverse (`validateRemoteDep` org) remoteDependencies
    Nothing -> pure remoteDependencies

  let renderedPath = toText root
      referenceLocators = locatorDependencies ++ (refToLocator <$> referencedDependencies)
      additional = toAdditionalData (NE.nonEmpty customDependencies) (NE.nonEmpty rdeps)
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
      , sourceUnitOriginPaths = [someBaseToOriginPath originPath]
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
  (archiveOrCLI, mode) <- getScanCfg org vendoredDepsOptions
  let uploadKind = orgFileUpload org
  let pathFilters = licenseScanPathFilters vendoredDepsOptions
  let scanner = case archiveOrCLI of
        ArchiveUpload -> archiveUploadSourceUnit $ vendoredDependencyScanModeToDependencyRebuild mode
        CLILicenseScan -> licenseScanSourceUnit mode pathFilters uploadKind

  when (archiveOrCLI == ArchiveUpload && isJust pathFilters) $
    fatalText "You have provided path filters in the vendoredDependencies.licenseScanPathFilters section of your .fossa.yml file. Path filters are not allowed when doing archive uploads."

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
refToLocator (Managed ManagedReferenceDependency{..}) =
  Locator
    { locatorFetcher = depTypeToFetcher locDepType
    , locatorProject = locDepName
    , locatorRevision = locDepVersion
    }
refToLocator (LinuxApkDebDep LinuxReferenceDependency{..}) =
  Locator
    { locatorFetcher = depTypeToFetcher locLinuxDepType
    , locatorProject = mkLinuxPackage locLinuxDepName locLinuxDepOS locLinuxDepOSVersion
    , locatorRevision = version
    }
  where
    version :: Maybe Text
    version = Just $ locLinuxDepArch <> "#" <> (fromMaybe "" locLinuxDepVersion)
refToLocator (LinuxRpmDep LinuxReferenceDependency{..} rpmEpoch) =
  Locator
    { locatorFetcher = depTypeToFetcher locLinuxDepType
    , locatorProject = mkLinuxPackage locLinuxDepName locLinuxDepOS locLinuxDepOSVersion
    , locatorRevision = version
    }
  where
    version :: Maybe Text
    version = Just $ locLinuxDepArch <> "#" <> epoch <> (fromMaybe "" locLinuxDepVersion)

    epoch :: Text
    epoch = maybe "" ((<> ":") . toText . show) rpmEpoch

mkLinuxPackage :: Text -> Text -> Text -> Text
mkLinuxPackage depName os osVersion = depName <> "#" <> os <> "#" <> osVersion

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
hasNoDeps ManualDependencies{..} =
  null referencedDependencies
    && null customDependencies
    && null vendoredDependencies
    && null remoteDependencies
    && null locatorDependencies

validateRemoteDep :: (Has Diagnostics sig m) => RemoteDependency -> Organization -> m RemoteDependency
validateRemoteDep r org =
  if locatorLen > maxLocatorLength
    then errCtx (RemoteDepLengthIsGtThanAllowedCtx r) $ errHelp (RemoteDepLengthIsGtThanAllowedHelp maxUrlRevLength) $ fatal $ RemoteDepLengthIsGtThanAllowedMessage getSourceLocation
    else pure r
  where
    orgId :: Text
    orgId = toText . show . organizationId $ org

    maxLocatorLength :: Int
    maxLocatorLength = 255

    locatorLen :: Int
    locatorLen = Text.length $ Text.intercalate "" [requiredChars, urlRevChars]

    requiredChars :: Text
    requiredChars = Text.intercalate "" ["url-private+", orgId, "/", "$"]

    urlRevChars :: Text
    urlRevChars = Text.intercalate "" [remoteUrl r, remoteVersion r]

    maxUrlRevLength :: Int
    maxUrlRevLength = maxLocatorLength - Text.length requiredChars

data RemoteDepLengthIsGtThanAllowed
  = RemoteDepLengthIsGtThanAllowedMessage SourceLocation
  | RemoteDepLengthIsGtThanAllowedCtx RemoteDependency
  | RemoteDepLengthIsGtThanAllowedHelp Int

instance ToDiagnostic RemoteDepLengthIsGtThanAllowed where
  renderDiagnostic :: RemoteDepLengthIsGtThanAllowed -> Errata
  renderDiagnostic (RemoteDepLengthIsGtThanAllowedMessage srcLoc) =
    errataSimple (Just "Remote-dependency length is exceeds limit") (createEmptyBlock srcLoc) Nothing
  renderDiagnostic (RemoteDepLengthIsGtThanAllowedCtx r) = do
    let header =
          renderIt $
            vsep
              [ pretty $ "The combined length of url and version is: " <> show urlRevLength
              , ""
              , indent 2 "You provided remote-dependency: "
              , indent 4 . pretty $ "Name: " <> remoteName r
              , indent 4 . pretty $ "Url: " <> remoteUrl r
              , indent 4 . pretty $ "Version: " <> remoteVersion r
              ]
    createErrataWithHeaderOnly header
    where
      urlRevLength :: Int
      urlRevLength = Text.length $ Text.intercalate "" [remoteUrl r, remoteVersion r]
  renderDiagnostic (RemoteDepLengthIsGtThanAllowedHelp maxLen) =
    createErrataWithHeaderOnly $ "Ensure that the combined length is below: " <> toText maxLen
