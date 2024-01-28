{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Analyze.Upload (
  mergeSourceAndLicenseUnits,
  uploadSuccessfulAnalysis,
  ScanUnits (..),
) where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Config.Analyze (JsonOutput (JsonOutput))
import App.Fossa.Reachability.Types (SourceUnitReachability)
import App.Fossa.Reachability.Upload (upload)
import App.Types (
  BaseDir (BaseDir),
  FullFileUploads (FullFileUploads),
  ProjectMetadata,
  ProjectRevision (..),
 )
import Control.Carrier.StickyLogger (StickyLogger, logSticky, runStickyLogger)
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (
  Diagnostics,
  context,
  fatalText,
  fromMaybeText,
  recover,
 )
import Control.Effect.FossaApiClient (
  FossaApiClient,
  PackageRevision (..),
  getOrganization,
  getProject,
  getSignedFirstPartyScanUrl,
  uploadAnalysis,
  uploadAnalysisWithFirstPartyLicenses,
  uploadContributors,
  uploadFirstPartyScanResult,
 )
import Control.Effect.Git (Git, fetchGitContributors)
import Control.Effect.Lift (Lift)
import Control.Monad (when)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Flag (Flag, fromFlag)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import Effect.Logger (
  Has,
  Logger,
  Pretty (pretty),
  Severity (SevInfo),
  logError,
  logInfo,
  logStdout,
  viaShow,
 )
import Fossa.API.Types (Organization (orgRequiresFullFileUploads), Project (projectIsMonorepo), UploadResponse (..))
import Path (Abs, Dir, Path)
import Srclib.Types (
  FullSourceUnit,
  LicenseSourceUnit (..),
  Locator (..),
  SourceUnit,
  licenseUnitToFullSourceUnit,
  renderLocator,
  sourceUnitToFullSourceUnit,
 )

data ScanUnits
  = SourceUnitOnly [SourceUnit]
  | LicenseSourceUnitOnly LicenseSourceUnit
  | SourceAndLicenseUnits [SourceUnit] LicenseSourceUnit
  deriving (Show)

-- units come from standard `fossa analyze`.
-- LicenseSourceUnit comes from running a first-party license scan on the project
-- merge these into an array before uploading to S3
mergeSourceAndLicenseUnits :: [SourceUnit] -> LicenseSourceUnit -> NE.NonEmpty FullSourceUnit
mergeSourceAndLicenseUnits units LicenseSourceUnit{..} =
  -- Replace with `NE.prependList fromLicenseUnits fromSourceUnits` after upgrading to > 4.16
  -- https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-List-NonEmpty.html#v:prependList
  foldr NE.cons fromLicenseUnits fromSourceUnits
  where
    fromSourceUnits = map sourceUnitToFullSourceUnit units
    fromLicenseUnits = NE.map licenseUnitToFullSourceUnit licenseSourceUnitLicenseUnits

uploadSuccessfulAnalysis ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has FossaApiClient sig m
  , Has Git sig m
  , Has Debug sig m
  ) =>
  BaseDir ->
  ProjectMetadata ->
  Flag JsonOutput ->
  ProjectRevision ->
  ScanUnits ->
  [SourceUnitReachability] ->
  m Locator
uploadSuccessfulAnalysis (BaseDir basedir) metadata jsonOutput revision scanUnits reachability =
  context "Uploading analysis" $ do
    logInfo ""
    logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
    logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
    let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
    logInfo ("Using branch: `" <> pretty branchText <> "`")

    dieOnMonorepoUpload revision
    void $ upload revision metadata reachability

    uploadResult <- case scanUnits of
      SourceUnitOnly units -> uploadAnalysis revision metadata units
      LicenseSourceUnitOnly licenseSourceUnit -> do
        org <- getOrganization
        let fullFileUploads = FullFileUploads $ orgRequiresFullFileUploads org
        let mergedUnits = mergeSourceAndLicenseUnits [] licenseSourceUnit
        runStickyLogger SevInfo $ uploadAnalysisWithFirstPartyLicensesToS3AndCore revision metadata mergedUnits fullFileUploads
      SourceAndLicenseUnits sourceUnits licenseSourceUnit -> do
        org <- getOrganization
        let fullFileUploads = FullFileUploads $ orgRequiresFullFileUploads org
        let mergedUnits = mergeSourceAndLicenseUnits sourceUnits licenseSourceUnit
        runStickyLogger SevInfo $ uploadAnalysisWithFirstPartyLicensesToS3AndCore revision metadata mergedUnits fullFileUploads
    let locator = uploadLocator uploadResult
    buildUrl <- getFossaBuildUrl revision locator
    traverse_
      logInfo
      [ "============================================================"
      , ""
      , "    View FOSSA Report:"
      , "    " <> pretty buildUrl
      , ""
      , "============================================================"
      ]
    traverse_ (\err -> logError $ "FOSSA error: " <> viaShow err) (uploadError uploadResult)
    -- Warn on contributor errors, never fail
    void . recover $ tryUploadContributors basedir (uploadLocator uploadResult)

    when (fromFlag JsonOutput jsonOutput) $ do
      summary <-
        context "Analysis ran successfully, but the server returned invalid metadata" $
          buildProjectSummary revision (uploadLocator uploadResult) buildUrl
      logStdout . decodeUtf8 $ Aeson.encode summary

    pure locator

uploadAnalysisWithFirstPartyLicensesToS3AndCore ::
  ( Has Diagnostics sig m
  , Has FossaApiClient sig m
  , Has StickyLogger sig m
  ) =>
  ProjectRevision ->
  ProjectMetadata ->
  NE.NonEmpty FullSourceUnit ->
  FullFileUploads ->
  m UploadResponse
uploadAnalysisWithFirstPartyLicensesToS3AndCore revision metadata mergedUnits fullFileUploads = do
  _ <- uploadAnalysisWithFirstPartyLicensesToS3 revision mergedUnits
  uploadAnalysisWithFirstPartyLicenses revision metadata fullFileUploads

uploadAnalysisWithFirstPartyLicensesToS3 ::
  ( Has Diagnostics sig m
  , Has FossaApiClient sig m
  , Has StickyLogger sig m
  ) =>
  ProjectRevision ->
  NE.NonEmpty FullSourceUnit ->
  m ()
uploadAnalysisWithFirstPartyLicensesToS3 revision mergedUnits = do
  signedURL <- getSignedFirstPartyScanUrl $ PackageRevision{packageVersion = projectRevision revision, packageName = projectName revision}
  logSticky $ "Uploading '" <> projectName revision <> "' to secure S3 bucket"
  uploadFirstPartyScanResult signedURL mergedUnits

dieOnMonorepoUpload :: (Has Diagnostics sig m, Has FossaApiClient sig m) => ProjectRevision -> m ()
dieOnMonorepoUpload revision = do
  project <- recover $ getProject revision
  when (maybe False projectIsMonorepo project) $
    fatalText "This project already exists as a monorepo project. Monorepo projects are no longer supported; please create a new project instead."

tryUploadContributors ::
  ( Has Diagnostics sig m
  , Has Git sig m
  , Has FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  Locator ->
  m ()
tryUploadContributors baseDir locator = do
  contributors <- fetchGitContributors baseDir
  uploadContributors locator contributors

-- | Build project summary JSON to be output to stdout
buildProjectSummary :: Has Diagnostics sig m => ProjectRevision -> Locator -> Text -> m Aeson.Value
buildProjectSummary project locator projectUrl = do
  revision <- fromMaybeText "Server returned an invalid project revision" $ locatorRevision locator
  pure $
    Aeson.object
      [ "project" .= locatorProject locator
      , "revision" .= revision
      , "branch" .= projectBranch project
      , "url" .= projectUrl
      , "id" .= renderLocator locator
      ]
