{-# LANGUAGE RecordWildCards #-}
module App.Fossa.Analyze.Upload (
  uploadSuccessfulAnalysis,
) where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Config.Analyze (JsonOutput (JsonOutput))
import App.Types (
  BaseDir (BaseDir),
  ProjectMetadata,
  ProjectRevision (..),
 )
import Control.Effect.Diagnostics (
  Diagnostics,
  context,
  fatalText,
  fromMaybeText,
  recover,
 )
import Control.Effect.FossaApiClient (
  FossaApiClient,
  getProject,
  uploadAnalysis,
  uploadContributors, PackageRevision (..), getSignedLicenseScanUrl,
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
  logError,
  logInfo,
  logStdout,
  viaShow,
 )
import Fossa.API.Types (Project (projectIsMonorepo), UploadResponse (uploadError, uploadLocator, UploadResponse), SignedURL, ApiOpts)
import Path (Abs, Dir, Path)
import Srclib.Types (
  Locator (..),
  SourceUnit,
  renderLocator,
  LicenseSourceUnit (..), FullSourceUnit, sourceUnitToFullSourceUnit, licenseUnitToFullSourceUnit,
 )
import qualified Control.Carrier.FossaApiClient.Internal.FossaAPIV1 as API
import Control.Carrier.Reader (Reader)
import Control.Effect.Reader (ask)
import Control.Carrier.StickyLogger (StickyLogger, logSticky)

-- units come from standard `fossa analyze`.
-- LicenseSourceUnit comes from running a first-party license scan on the project
-- merge these into an array before uploading to S3
mergeSourceAndLicenseUnits :: NE.NonEmpty SourceUnit -> LicenseSourceUnit -> [FullSourceUnit]
mergeSourceAndLicenseUnits units LicenseSourceUnit{..} =
  fromSourceUnits ++ fromLicenseUnits
  where
    fromSourceUnits = map sourceUnitToFullSourceUnit $ NE.toList units
    fromLicenseUnits = map licenseUnitToFullSourceUnit $ NE.toList licenseSourceUnitLicenseUnits

uploadSuccessfulAnalysis ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has FossaApiClient sig m
  , Has Git sig m
  ) =>
  BaseDir ->
  ProjectMetadata ->
  Flag JsonOutput ->
  ProjectRevision ->
  NE.NonEmpty SourceUnit ->
  Maybe LicenseSourceUnit ->
  m Locator
uploadSuccessfulAnalysis (BaseDir basedir) metadata jsonOutput revision units licenseUnits =
  context "Uploading analysis" $ do
    logInfo ""
    logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
    logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
    let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
    logInfo ("Using branch: `" <> pretty branchText <> "`")

    dieOnMonorepoUpload revision

    uploadResult <- case licenseUnits of
      Nothing -> uploadAnalysis revision metadata units
      Just licenses -> do
        let mergedUnits = mergeSourceAndLicenseUnits units licenses
        uploadFirstPartyAnalysisToS3AndCore revision metadata mergedUnits
        -- uploadAnalysis revision metadata mergedUnits
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

uploadFirstPartyAnalysisToS3AndCore ::
  ( Has Diagnostics sig m
  , Has FossaApiClient sig m
  , Has StickyLogger sig m
  ) => ProjectRevision -> ProjectMetadata -> [FullSourceUnit] -> m UploadResponse
uploadFirstPartyAnalysisToS3AndCore revision metadata mergedUnits = do
  uploadFirstPartyAnalysisToS3 revision mergedUnits

uploadFirstPartyAnalysisToS3 ::
  ( Has Diagnostics sig m
  , Has FossaApiClient sig m
  , Has StickyLogger sig m
  ) => ProjectRevision -> [FullSourceUnit] -> m UploadResponse
uploadFirstPartyAnalysisToS3 revision mergedUnits = do
  -- TODO: change this to getSignedFirstPartyScanUrl
  signedURL <- getSignedLicenseScanUrl $ PackageRevision{packageVersion = projectRevision revision, packageName = projectName revision}
  logSticky $ "Uploading '" <> projectName revision <> "' to secure S3 bucket"
  -- TODO: copy/paste/modify of uploadLicenseScanResult
  uploadFirstPartyScanResult signedURL mergedUnits

dieOnMonorepoUpload :: (Has Diagnostics sig m, Has FossaApiClient sig m) => ProjectRevision -> m ()
dieOnMonorepoUpload revision = do
  project <- recover $ getProject revision
  when (maybe False projectIsMonorepo project) $
    fatalText "This project already exists as a monorepo project. Perhaps you meant to supply '--experimental-enable-monorepo', or meant to run 'fossa vps analyze' instead?"

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
