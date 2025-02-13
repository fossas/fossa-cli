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
  LocatorDependency (..),
  FoundDepsFile (..),
  analyzeFossaDepsFile,
  findAndReadFossaDepsFile,
  findFossaDepsFile,
  readFoundDeps,
  getScanCfg,
  collectInteriorLabels,
)
where

import App.Fossa.ArchiveUploader (archiveUploadSourceUnit)
import App.Fossa.Config.Analyze (
  VendoredDependencyOptions (..),
 )
import App.Fossa.Config.Common (validateFile)
import App.Fossa.DependencyMetadata (DependencyMetadata (..))
import App.Fossa.LicenseScanner (licenseScanSourceUnit)
import App.Fossa.VendoredDependency (
  VendoredDependency (..),
  VendoredDependencyScanMode (..),
  arcToLocator,
  forceVendoredToArchive,
  vendoredDependencyScanModeToDependencyRebuild,
 )
import Control.Applicative ((<|>))
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (Diagnostics, context, errCtx, errHelp, fatal, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
import Control.Effect.Lift (Has, Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Control.Monad (unless, when)
import Data.Aeson (
  FromJSON (parseJSON),
  Value (Null, Object),
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson.Extra (TextLike (unTextLike), forbidMembers, neText)
import Data.Aeson.Types (Object, Parser, prependFailure)
import Data.Error (SourceLocation, createEmptyBlock, createErrataWithHeaderOnly, getSourceLocation)
import Data.Functor.Extra ((<$$>))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.String.Conversion (toString, toText)
import Data.Text (Text, toLower)
import Data.Text qualified as Text
import DepTypes (DepType (..))
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Effect.Exec (Exec)
import Effect.Logger (Logger, indent, pretty, renderIt, vsep)
import Effect.ReadFS (ReadFS, doesFileExist, readContentsJson, readContentsYaml)
import Errata (Errata (..), errataSimple)
import Fossa.API.Types (ApiOpts, OrgId, Organization (..), orgFileUpload)
import Path (Abs, Dir, File, Path, mkRelFile, (</>))
import Path.Extra (tryMakeRelative)
import Srclib.Converter (depTypeToFetcher)
import Srclib.Types (AdditionalDepData (..), Locator (..), ProvidedPackageLabel, SourceRemoteDep (..), SourceUnit (..), SourceUnitBuild (..), SourceUnitDependency (SourceUnitDependency), SourceUserDefDep (..), buildProvidedPackageLabels, parseLocator, renderLocator, someBaseToOriginPath)
import System.FilePath (takeExtension)
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

  -- Some manual deps, such as remote dependencies in source unit cannot be
  -- validated without the org data.
  org <- case maybeApiOpts of
    Just apiOpts -> Just <$> runFossaApiClient apiOpts getOrganization
    Nothing -> pure Nothing

  -- Labels are provided by users attached to actual dependencies,
  -- but are collected into the root of the source unit for reporting to FOSSA.
  let labels = collectInteriorLabels (organizationId <$> org) manualDeps

  archiveLocators <- case (maybeApiOpts, NE.nonEmpty vendoredDependencies) of
    (Just apiOpts, Just vdeps) -> NE.toList <$> runFossaApiClient apiOpts (scanAndUpload root vdeps vendoredDepsOptions)
    (Nothing, Just vdeps) -> pure $ noSourceUnits $ NE.toList vdeps
    -- Don't do anything if there are no vendored deps.
    (_, Nothing) -> pure []

  -- Some manual deps, such as remote dependencies in source unit cannot be
  -- validated without endpoint interactions.
  rdeps <- case maybeApiOpts of
    Just apiOpts -> runFossaApiClient apiOpts $ do
      org' <- maybe getOrganization pure org
      traverse (`validateRemoteDep` org') remoteDependencies
    Nothing -> pure remoteDependencies

  let renderedPath = toText root
      referenceLocators = (extractLocator <$> locatorDependencies) ++ (refToLocator <$> referencedDependencies)
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
      , sourceUnitNoticeFiles = []
      , sourceUnitOriginPaths = [someBaseToOriginPath originPath]
      , additionalData = additional
      , sourceUnitLabels = buildProvidedPackageLabels labels
      }

-- | Collect labels from dependencies into one big map.
-- The key of the map is the locator to which the labels correspond.
collectInteriorLabels :: Maybe OrgId -> ManualDependencies -> Map Text [ProvidedPackageLabel]
collectInteriorLabels org ManualDependencies{..} = do
  let referenced = map refDepToLabel referencedDependencies
  let vendored = map vendDepToLabel vendoredDependencies
  let custom = map customDepToLabel customDependencies
  let remote = map (remoteDepToLabel org) remoteDependencies
  let locator = map locatorDepToLabel locatorDependencies
  group . catMaybes $ referenced <> vendored <> custom <> remote <> locator
  where
    liftMaybe :: (a, Maybe b) -> Maybe (a, b)
    liftMaybe (_, Nothing) = Nothing
    liftMaybe (a, Just b) = Just (a, b)

    group :: (Ord a) => [(a, [b])] -> Map a [b]
    group = Map.fromListWith (++)

    renderCustomDepLocator :: CustomDependency -> Text
    renderCustomDepLocator CustomDependency{..} = depTypeToText UserType <> "+" <> customName <> "$" <> customVersion

    renderRemoteDepLocator :: Maybe OrgId -> RemoteDependency -> Text
    renderRemoteDepLocator (Just orgId) RemoteDependency{..} = ("url-private+" <> toText orgId <> "/" <> remoteUrl <> "$" <> remoteVersion)
    renderRemoteDepLocator Nothing RemoteDependency{..} = ("url-private+" <> remoteUrl <> "$" <> remoteVersion)

    refDepToLabel :: ReferencedDependency -> Maybe (Text, [ProvidedPackageLabel])
    refDepToLabel dep@(Managed ManagedReferenceDependency{..}) = liftMaybe (toText $ refToLocator dep, locDepLabels)
    refDepToLabel dep@(LinuxApkDebDep LinuxReferenceDependency{..}) = liftMaybe (toText $ refToLocator dep, locLinuxDepLabels)
    refDepToLabel dep@(LinuxRpmDep LinuxReferenceDependency{..} _) = liftMaybe (toText $ refToLocator dep, locLinuxDepLabels)

    vendDepToLabel :: VendoredDependency -> Maybe (Text, [ProvidedPackageLabel])
    vendDepToLabel dep@VendoredDependency{..} = liftMaybe (toText . arcToLocator $ forceVendoredToArchive dep, vendoredLabels)

    customDepToLabel :: CustomDependency -> Maybe (Text, [ProvidedPackageLabel])
    customDepToLabel dep@CustomDependency{..} = liftMaybe (renderCustomDepLocator dep, customLabels)

    remoteDepToLabel :: Maybe OrgId -> RemoteDependency -> Maybe (Text, [ProvidedPackageLabel])
    remoteDepToLabel orgId dep@RemoteDependency{..} = liftMaybe (renderRemoteDepLocator orgId dep, remoteLabels)

    locatorDepToLabel :: LocatorDependency -> Maybe (Text, [ProvidedPackageLabel])
    locatorDepToLabel (LocatorDependencyPlain _) = Nothing
    locatorDepToLabel (LocatorDependencyStructured locator labels) = liftMaybe (renderLocator locator, labels)

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
    epoch = maybe "" ((<> ":") . toText) rpmEpoch

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

-- TODO: Change these to Maybe NonEmpty
data ManualDependencies = ManualDependencies
  { referencedDependencies :: [ReferencedDependency]
  , customDependencies :: [CustomDependency]
  , vendoredDependencies :: [VendoredDependency]
  , remoteDependencies :: [RemoteDependency]
  , locatorDependencies :: [LocatorDependency]
  }
  deriving (Eq, Ord, Show)

data LocatorDependency
  = LocatorDependencyPlain Locator
  | LocatorDependencyStructured Locator (Maybe [ProvidedPackageLabel])
  deriving (Eq, Ord, Show)

extractLocator :: LocatorDependency -> Locator
extractLocator (LocatorDependencyPlain locator) = locator
extractLocator (LocatorDependencyStructured locator _) = locator

data ReferencedDependency
  = Managed ManagedReferenceDependency
  | LinuxApkDebDep LinuxReferenceDependency
  | LinuxRpmDep LinuxReferenceDependency (Maybe Text)
  deriving (Eq, Ord, Show)

data ManagedReferenceDependency = ManagedReferenceDependency
  { locDepName :: Text
  , locDepType :: DepType
  , locDepVersion :: Maybe Text
  , locDepLabels :: Maybe [ProvidedPackageLabel]
  }
  deriving (Eq, Ord, Show)

data LinuxReferenceDependency = LinuxReferenceDependency
  { locLinuxDepName :: Text
  , locLinuxDepType :: DepType
  , locLinuxDepVersion :: Maybe Text
  , locLinuxDepArch :: Text
  , locLinuxDepOS :: Text
  , locLinuxDepOSVersion :: Text
  , locLinuxDepLabels :: Maybe [ProvidedPackageLabel]
  }
  deriving (Eq, Ord, Show)

data CustomDependency = CustomDependency
  { customName :: Text
  , customVersion :: Text
  , customLicense :: Text
  , customMetadata :: Maybe DependencyMetadata
  , customLabels :: Maybe [ProvidedPackageLabel]
  }
  deriving (Eq, Ord, Show)

data RemoteDependency = RemoteDependency
  { remoteName :: Text
  , remoteVersion :: Text
  , remoteUrl :: Text
  , remoteMetadata :: Maybe DependencyMetadata
  , remoteLabels :: Maybe [ProvidedPackageLabel]
  }
  deriving (Eq, Ord, Show)

instance FromJSON LocatorDependency where
  parseJSON val = parseLabeled val <|> parsePlain val
    where
      parsePlain :: Value -> Parser LocatorDependency
      parsePlain = withText "locator" $ pure . LocatorDependencyPlain . parseLocator

      parseLabeled :: Value -> Parser LocatorDependency
      parseLabeled = withObject "locator" $ \obj ->
        LocatorDependencyStructured <$> obj .: "locator" <*> obj .:? "labels"

instance FromJSON ManualDependencies where
  parseJSON (Object obj) =
    ManualDependencies
      <$ (obj .:? "version" >>= isMissingOr1)
      <*> (obj .:? "referenced-dependencies" .!= [])
      <*> (obj .:? "custom-dependencies" .!= [])
      <*> (obj .:? "vendored-dependencies" .!= [])
      <*> (obj .:? "remote-dependencies" .!= [])
      <*> (obj .:? "locator-dependencies" .!= [])
    where
      isMissingOr1 :: Maybe Int -> Parser ()
      isMissingOr1 (Just x) | x /= 1 = fail $ "Invalid fossa-deps version: " <> show x
      isMissingOr1 _ = pure ()
  parseJSON (Null) = pure $ ManualDependencies mempty mempty mempty mempty mempty
  parseJSON other = fail $ "Expected object or Null for ManualDependencies, but got: " <> show other

depTypeParser :: Text -> Parser DepType
depTypeParser text = case depTypeFromText text of
  Just t -> pure t
  Nothing -> fail $ "dep type: " <> toString text <> " not supported"

instance FromJSON ReferencedDependency where
  parseJSON = withObject "ReferencedDependency" $ \obj -> do
    depType <- parseDepType obj
    case depType of
      LinuxRPM -> parseRpmDependency obj depType
      LinuxAPK -> parseApkOrDebDependency obj depType
      LinuxDEB -> parseApkOrDebDependency obj depType
      _ -> parseManagedDependency obj depType
    where
      parseDepType :: Object -> Parser DepType
      parseDepType obj = obj .: "type" >>= depTypeParser

      parseManagedDependency :: Object -> DepType -> Parser ReferencedDependency
      parseManagedDependency obj depType =
        Managed
          <$> ( ManagedReferenceDependency
                  <$> (obj `neText` "name")
                  <*> pure depType
                  <*> (unTextLike <$$> obj .:? "version")
                  <*> obj .:? "labels"
                  <* forbidNonRefDepFields obj
                  <* forbidLinuxFields depType obj
                  <* forbidEpoch depType obj
              )

      parseApkOrDebDependency :: Object -> DepType -> Parser ReferencedDependency
      parseApkOrDebDependency obj depType =
        LinuxApkDebDep
          <$> parseLinuxDependency obj depType
          <* forbidNonRefDepFields obj
          <* forbidEpoch depType obj

      parseRpmDependency :: Object -> DepType -> Parser ReferencedDependency
      parseRpmDependency obj depType =
        LinuxRpmDep
          <$> parseLinuxDependency obj depType
          <*> (unTextLike <$$> obj .:? "epoch")
          <* forbidNonRefDepFields obj

      parseLinuxDependency :: Object -> DepType -> Parser LinuxReferenceDependency
      parseLinuxDependency obj depType =
        LinuxReferenceDependency
          <$> (obj `neText` "name")
          <*> pure depType
          <*> (unTextLike <$$> obj .:? "version")
          <*> parseArch obj
          <*> parseOS obj
          <*> parseOSVersion obj
          <*> obj .:? "labels"
      parseArch :: Object -> Parser Text
      parseArch obj = requiredFieldMsg "arch" $ obj .: "arch"

      parseOSVersion :: Object -> Parser Text
      parseOSVersion obj = requiredFieldMsg "osVersion" (unTextLike <$> obj .: "osVersion")

      parseOS :: Object -> Parser Text
      parseOS obj = do
        os <- requiredFieldMsg "os" $ obj .: "os"
        unless (toLower os `elem` supportedOSs)
          $ fail
            . toString
          $ "Provided os: "
            <> (toLower os)
            <> " is not supported! Please provide oneOf: "
            <> Text.intercalate ", " supportedOSs
        pure os

      requiredFieldMsg :: String -> Parser a -> Parser a
      requiredFieldMsg field =
        prependFailure $ field <> " is required field for reference dependency (of dependency type: apk, deb, rpm-generic): "

      forbidLinuxFields :: DepType -> Object -> Parser ()
      forbidLinuxFields depType =
        forbidMembers
          ("referenced dependencies (of dependency type: " <> depTypeToText depType <> ")")
          [ "os"
          , "osVersion"
          , "arch"
          ]

      forbidEpoch :: DepType -> Object -> Parser ()
      forbidEpoch depType =
        forbidMembers
          ("referenced dependencies (of dependency type: " <> depTypeToText depType <> ")")
          ["epoch"]

      forbidNonRefDepFields :: Object -> Parser ()
      forbidNonRefDepFields =
        forbidMembers
          "referenced dependencies"
          [ "license"
          , "description"
          , "url"
          , "path"
          ]

instance FromJSON CustomDependency where
  parseJSON = withObject "CustomDependency" $ \obj ->
    CustomDependency
      <$> (obj `neText` "name")
      <*> (unTextLike <$> obj `neText` "version")
      <*> (obj `neText` "license")
      <*> obj .:? "metadata"
      <*> obj .:? "labels"
      <* forbidMembers "custom dependencies" ["type", "path", "url"] obj

instance FromJSON RemoteDependency where
  parseJSON = withObject "RemoteDependency" $ \obj -> do
    RemoteDependency
      <$> (obj `neText` "name")
      <*> (unTextLike <$> obj `neText` "version")
      <*> (obj `neText` "url")
      <*> obj .:? "metadata"
      <*> obj .:? "labels"
      <* forbidMembers "remote dependencies" ["license", "path", "type"] obj

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

-- Parse supported dependency types into their respective type or return Nothing.
depTypeFromText :: Text -> Maybe DepType
depTypeFromText text = case text of
  "bower" -> Just BowerType
  "cargo" -> Just CargoType
  "carthage" -> Just CarthageType
  "composer" -> Just ComposerType
  "cpan" -> Just CpanType
  "cran" -> Just CranType
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
  "apk" -> Just LinuxAPK
  "deb" -> Just LinuxDEB
  "rpm-generic" -> Just LinuxRPM
  _ -> Nothing -- unsupported dep, need to respond with an error and skip this dependency

depTypeToText :: DepType -> Text
depTypeToText depType = case depType of
  BowerType -> "bower"
  CargoType -> "cargo"
  CarthageType -> "carthage"
  ComposerType -> "composer"
  CpanType -> "cpan"
  CranType -> "cran"
  GemType -> "gem"
  GitType -> "git"
  GoType -> "go"
  HackageType -> "hackage"
  HexType -> "hex"
  MavenType -> "maven"
  NodeJSType -> "npm"
  NuGetType -> "nuget"
  PipType -> "pypi"
  PodType -> "cocoapods"
  URLType -> "url"
  SwiftType -> "swift"
  LinuxAPK -> "apk"
  LinuxDEB -> "deb"
  LinuxRPM -> "rpm"
  UserType -> "user"
  other -> toText . show $ other

-- | Distro OS supported by FOSSA.
-- If you update this, please make sure to update /docs/references/files/fossa-deps.schema.json
supportedOSs :: [Text]
supportedOSs =
  [ "alpine"
  , "centos"
  , "debian"
  , "redhat"
  , "ubuntu"
  , "oraclelinux"
  , "busybox"
  , "sles"
  , "fedora"
  , "rocky"
  ]
