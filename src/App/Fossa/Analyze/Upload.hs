module App.Fossa.Analyze.Upload (
  uploadSuccessfulAnalysis,
) where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Config.Analyze (JsonOutput (JsonOutput))
import App.Fossa.FossaAPIV1 (
  getProject,
  uploadAnalysis,
  uploadContributors,
 )
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
import Control.Effect.Lift (Lift)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Flag (Flag, fromFlag)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import Effect.Exec (Exec, runExecIO)
import Effect.Logger (
  Has,
  Logger,
  Pretty (pretty),
  logError,
  logInfo,
  logStdout,
  viaShow,
 )
import Fossa.API.Types (ApiOpts, Project (projectIsMonorepo), UploadResponse (uploadError, uploadLocator))
import Path (Abs, Dir, Path)
import Srclib.Types (
  Locator (locatorProject, locatorRevision),
  SourceUnit,
  parseLocator,
 )
import VCS.Git (fetchGitContributors)

uploadSuccessfulAnalysis ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  ) =>
  BaseDir ->
  ApiOpts ->
  ProjectMetadata ->
  Flag JsonOutput ->
  ProjectRevision ->
  NE.NonEmpty SourceUnit ->
  m Locator
uploadSuccessfulAnalysis (BaseDir basedir) apiOpts metadata jsonOutput revision units = context "Uploading analysis" $ do
  logInfo ""
  logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
  logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
  let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
  logInfo ("Using branch: `" <> pretty branchText <> "`")

  dieOnMonorepoUpload apiOpts revision

  uploadResult <- uploadAnalysis apiOpts revision metadata units
  let locator = parseLocator $ uploadLocator uploadResult
  buildUrl <- getFossaBuildUrl revision apiOpts locator
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
  void . recover . runExecIO $ tryUploadContributors basedir apiOpts (uploadLocator uploadResult)

  if fromFlag JsonOutput jsonOutput
    then do
      summary <-
        context "Analysis ran successfully, but the server returned invalid metadata" $
          buildProjectSummary revision (uploadLocator uploadResult) buildUrl
      logStdout . decodeUtf8 $ Aeson.encode summary
    else pure ()

  pure locator

dieOnMonorepoUpload :: (Has Diagnostics sig m, Has (Lift IO) sig m) => ApiOpts -> ProjectRevision -> m ()
dieOnMonorepoUpload apiOpts revision = do
  project <- recover $ getProject apiOpts revision
  if maybe False projectIsMonorepo project
    then fatalText "This project already exists as a monorepo project. Perhaps you meant to supply '--experimental-enable-monorepo', or meant to run 'fossa vps analyze' instead?"
    else pure ()

tryUploadContributors ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs Dir ->
  ApiOpts ->
  -- | Locator
  Text ->
  m ()
tryUploadContributors baseDir apiOpts locator = do
  contributors <- fetchGitContributors baseDir
  uploadContributors apiOpts locator contributors

-- | Build project summary JSON to be output to stdout
buildProjectSummary :: Has Diagnostics sig m => ProjectRevision -> Text -> Text -> m Aeson.Value
buildProjectSummary project projectLocator projectUrl = do
  let locator = parseLocator projectLocator
  revision <- fromMaybeText "Server returned an invalid project revision" $ locatorRevision locator
  pure $
    Aeson.object
      [ "project" .= locatorProject locator
      , "revision" .= revision
      , "branch" .= projectBranch project
      , "url" .= projectUrl
      , "id" .= projectLocator
      ]
