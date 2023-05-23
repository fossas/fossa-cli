module App.Fossa.Analyze.Upload (
  uploadSuccessfulAnalysis,
) where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Config.Analyze (JsonOutput (JsonOutput))
import App.Types (
  BaseDir (BaseDir),
  ProjectMetadata,
  ProjectRevision (projectBranch, projectName, projectRevision),
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
  uploadContributors,
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
import Fossa.API.Types (Project (projectIsMonorepo), UploadResponse (uploadError, uploadLocator))
import Path (Abs, Dir, Path)
import Srclib.Types (
  Locator (locatorProject, locatorRevision),
  SourceUnit,
  renderLocator,
 )

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
  m Locator
uploadSuccessfulAnalysis (BaseDir basedir) metadata jsonOutput revision units =
  context "Uploading analysis" $ do
    logInfo ""
    logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
    logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
    let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
    logInfo ("Using branch: `" <> pretty branchText <> "`")

    dieOnMonorepoUpload revision

    uploadResult <- uploadAnalysis revision metadata units
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
