{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.PreflightChecks (
  PreflightCommandChecks (..),
  preflightChecks,
  guardWithPreflightChecks,
) where

import App.Docs (apiKeyUrl, apiTokenDocsUrl, fossaConfigDocsUrl, rolesDocsUrl)
import App.Support (reportDefectMsg)
import App.Types (ProjectMetadata, ProjectRevision)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics (Diagnostics, errCtx, errDoc, errHelp)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.Stack (context)
import Control.Effect.Diagnostics (ToDiagnostic, errSupport, fatal, fatalOnIOException)
import Control.Effect.FossaApiClient (FossaApiClient, getCustomBuildPermissions, getOrganization, getTokenType)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (void, when)
import Data.Error (createErrataWithHeaderOnly)
import Data.Text
import Data.Text.IO qualified as TIO
import Diag.Diagnostic (ToDiagnostic (..))
import Effect.Logger (renderIt)
import Errata (Errata (..))
import Fossa.API.Types (ApiOpts, CustomBuildUploadPermissions (..), Organization (..), ProjectPermissionStatus (..), ReleaseGroupPermissionStatus (..), Subscription (..), TokenType (..), TokenTypeResponse (..))
import Path (
  File,
  Path,
  Rel,
  fromAbsFile,
  mkRelFile,
  (</>),
 )
import Path.IO (getTempDir, removeFile)

data PreflightCommandChecks
  = AnalyzeChecks ProjectRevision ProjectMetadata
  | TestChecks
  | ReportChecks
  | AssertUserDefinedBinariesChecks

guardWithPreflightChecks ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  ApiOpts ->
  PreflightCommandChecks ->
  m ()
guardWithPreflightChecks apiOpts cmd = ignoreDebug $ runFossaApiClient apiOpts $ preflightChecks cmd

preflightChecks ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has FossaApiClient sig m
  ) =>
  PreflightCommandChecks ->
  m ()
preflightChecks cmd = context "preflight-checks" $ do
  -- Check for writing to temp dir
  tmpDir <- sendIO getTempDir
  fatalOnIOException "Failed to write to temp directory" . sendIO $ TIO.writeFile (fromAbsFile $ tmpDir </> preflightCheckFileName) "Writing to temp dir"
  sendIO $ removeFile (tmpDir </> preflightCheckFileName)

  -- Check for valid API Key and if user can connect to fossa app
  org <- errHelp InvalidApiKeyErr $ errDoc apiKeyUrl getOrganization

  when (orgSupportsPreflightChecks org) $
    void $
      errSupport (renderIt reportDefectMsg) $
        case cmd of
          TestChecks -> do
            tokenType <- getTokenType
            fullAccessTokenCheck tokenType
          AnalyzeChecks rev metadata -> do
            customBuildPermissions <- getCustomBuildPermissions rev metadata
            uploadBuildPermissionsCheck customBuildPermissions
          ReportChecks -> do
            tokenType <- getTokenType
            fullAccessTokenCheck tokenType
            premiumSubscriptionCheck org
          _ -> pure ()

uploadBuildPermissionsCheck :: Has Diagnostics sig m => CustomBuildUploadPermissions -> m ()
uploadBuildPermissionsCheck CustomBuildUploadPermissions{..} =
  case maybeReleaseGroupPermissionStatus of
    Just releaseGroupPermissionStatus ->
      case releaseGroupPermissionStatus of
        ValidReleaseGroupPermission -> pure ()
        InvalidEditReleaseGroupPermission ->
          errDoc rolesDocsUrl
            . errHelp permissionHelpMsg
            $ fatal EditReleaseGroupPermissionErr
        InvalidCreateTeamProjectsForReleaseGroupPermission ->
          errDoc rolesDocsUrl
            . errHelp permissionHelpMsg
            $ fatal CreateTeamProjectsForReleaseGroupPermissionErr
    Nothing ->
      case projectPermissionStatus of
        ValidProjectPermission -> pure ()
        InvalidEditProjectPermission ->
          errDoc rolesDocsUrl
            . errHelp permissionHelpMsg
            $ fatal EditProjectPermissionErr
        InvalidCreateProjectPermission ->
          errDoc rolesDocsUrl
            . errHelp permissionHelpMsg
            $ fatal CreateProjectPermissionErr
        InvalidCreateTeamProjectPermission ->
          errDoc rolesDocsUrl
            . errHelp permissionHelpMsg
            $ fatal CreateTeamProjectPermissionErr
        InvalidCreateProjectOnlyToTeamPermission ->
          errDoc fossaConfigDocsUrl
            . errHelp ("Ensure that you have specified a team to add this project to" :: Text)
            $ fatal CreateProjectOnlyToTeamPermissionErr
  where
    permissionHelpMsg :: Text
    permissionHelpMsg = "Contact your FOSSA organization admin to grant you proper permissions"

fullAccessTokenCheck :: Has Diagnostics sig m => TokenTypeResponse -> m ()
fullAccessTokenCheck TokenTypeResponse{..} = case tokenType of
  Push ->
    errHelp ("Ensure you are using a `Full Access` API token" :: Text)
      . errDoc apiTokenDocsUrl
      . errCtx ("You are currently using a `Push Only` API token" :: Text)
      $ fatal TokenTypeErr
  _ -> pure ()

premiumSubscriptionCheck :: Has Diagnostics sig m => Organization -> m ()
premiumSubscriptionCheck Organization{..} = case orgSubscription of
  Free ->
    errHelp ("To proceed, please upgrade your subscription" :: Text)
      . errCtx ("You currently have a free subscription" :: Text)
      $ fatal SubscriptionTypeErr
  _ -> pure ()

preflightCheckFileName :: Path Rel File
preflightCheckFileName = $(mkRelFile "preflight-check.txt")

data InvalidApiKeyErr = InvalidApiKeyErr
instance ToDiagnostic InvalidApiKeyErr where
  renderDiagnostic :: InvalidApiKeyErr -> Errata
  renderDiagnostic InvalidApiKeyErr =
    createErrataWithHeaderOnly "Ensure that you are using a valid FOSSA_API_KEY. Refer to the provided documentation for guidance on how to generate and retrieve your API key."

data TokenTypeErr = TokenTypeErr
instance ToDiagnostic TokenTypeErr where
  renderDiagnostic :: TokenTypeErr -> Errata
  renderDiagnostic TokenTypeErr =
    Errata (Just "Invalid API token type") [] $ Just "The action you are trying to perform requires a `Full Access` API token"

data SubscriptionTypeErr = SubscriptionTypeErr
instance ToDiagnostic SubscriptionTypeErr where
  renderDiagnostic :: SubscriptionTypeErr -> Errata
  renderDiagnostic SubscriptionTypeErr =
    Errata (Just "Invalid subscription type") [] $ Just "The action you are trying to perform requires a premium subscription"

projectPermissionErrHeader :: Text
projectPermissionErrHeader = "Invalid project permission"

data ProjectPermissionErr
  = CreateProjectPermissionErr
  | EditProjectPermissionErr
  | CreateTeamProjectPermissionErr
  | CreateProjectOnlyToTeamPermissionErr

instance ToDiagnostic ProjectPermissionErr where
  renderDiagnostic :: ProjectPermissionErr -> Errata
  renderDiagnostic CreateProjectPermissionErr =
    Errata (Just projectPermissionErrHeader) [] $ Just "You do not have permission to create projects for your Organization"
  renderDiagnostic EditProjectPermissionErr =
    Errata (Just projectPermissionErrHeader) [] $ Just "You do not have permission to edit projects for your Organization"
  renderDiagnostic CreateTeamProjectPermissionErr =
    Errata (Just projectPermissionErrHeader) [] $ Just "You do not have permission to create projects for the specified team"
  renderDiagnostic CreateProjectOnlyToTeamPermissionErr =
    Errata (Just projectPermissionErrHeader) [] $ Just "You only have permission to create projects for your team"

releaseGroupPermissionErrHeader :: Text
releaseGroupPermissionErrHeader = "Invalid release group permission"

data ReleaseGroupPermissionErr
  = EditReleaseGroupPermissionErr
  | CreateTeamProjectsForReleaseGroupPermissionErr

instance ToDiagnostic ReleaseGroupPermissionErr where
  renderDiagnostic :: ReleaseGroupPermissionErr -> Errata
  renderDiagnostic EditReleaseGroupPermissionErr =
    Errata (Just releaseGroupPermissionErrHeader) [] $ Just "You do not have permission to edit the specified release group"
  renderDiagnostic CreateTeamProjectsForReleaseGroupPermissionErr =
    Errata (Just releaseGroupPermissionErrHeader) [] $ Just "You do not have permission to add projects to all teams belonging to the specified release group"
