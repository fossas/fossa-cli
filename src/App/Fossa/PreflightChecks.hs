{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.PreflightChecks (
  PreflightCommandChecks (..),
  preflightChecks,
  guardWithPreflightChecks,
) where

import App.Docs (apiKeyUrl)
import App.Types (ProjectMetadata, ProjectRevision)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics (Diagnostics, errCtx)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.Stack (context)
import Control.Effect.Diagnostics (ToDiagnostic, fatalOnIOException, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, getCustomBuildPermissions, getOrganization, getSubscription, getTokenType)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (void, when)
import Data.Text.IO qualified as TIO
import Diag.Diagnostic (ToDiagnostic (..))
import Effect.Logger (Logger, logDebug, pretty, vsep)
import Fossa.API.Types (ApiOpts, CustomBuildUploadPermissions (..), Organization (orgSupportsPreflightChecks), ProjectPermissionStatus (..), ReleaseGroupPermissionStatus (..), Subscription (..), SubscriptionResponse (..), TokenType (..), TokenTypeResponse (..))
import Path (
  File,
  Path,
  Rel,
  fromAbsFile,
  mkRelFile,
  (</>),
 )
import Path.IO (getTempDir, removeFile)
import Text.Pretty.Simple (pShow)

data PreflightCommandChecks
  = AnalyzeChecks ProjectRevision ProjectMetadata
  | TestChecks
  | ReportChecks
  | ConatinerAnalzyeChecks
  | ContainerTestChecks
  | AssertUserDefinedBinariesChecks

guardWithPreflightChecks ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  ApiOpts ->
  PreflightCommandChecks ->
  m ()
guardWithPreflightChecks apiOpts cmd = ignoreDebug $ runFossaApiClient apiOpts $ preflightChecks cmd

preflightChecks ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has FossaApiClient sig m
  , Has Logger sig m
  ) =>
  PreflightCommandChecks ->
  m ()
preflightChecks cmd = context "preflight-checks" $ do
  -- Check for writing to temp dir
  tmpDir <- sendIO getTempDir
  fatalOnIOException "Failed to write to temp directory" . sendIO $ TIO.writeFile (fromAbsFile $ tmpDir </> preflightCheckFileName) "Writing to temp dir"
  sendIO $ removeFile (tmpDir </> preflightCheckFileName)

  -- Check for valid API Key and if user can connect to fossa app

  org <- errCtx InvalidApiKeyErr getOrganization

  when (orgSupportsPreflightChecks org) $
    void $
      case cmd of
        TestChecks -> do
          tokenType <- getTokenType
          fullAccessTokenCheck tokenType
        AnalyzeChecks rev metadata -> do
          customBuildPermissions <- getCustomBuildPermissions rev metadata
          logDebug $ "This is the permission data" <> pretty (pShow (customBuildPermissions))
          uploadBuildPermissionsCheck customBuildPermissions
        _ -> do
          tokenType <- getTokenType
          subscription <- getSubscription
          fullAccessTokenCheck tokenType
          premiumSubscriptionCheck subscription

  void $ errCtx InvalidApiKeyErr getOrganization

uploadBuildPermissionsCheck :: Has Diagnostics sig m => CustomBuildUploadPermissions -> m ()
uploadBuildPermissionsCheck CustomBuildUploadPermissions{..} =
  case maybeReleaseGroupPermissionStatus of
    Just releaseGroupPermissionStatus ->
      case releaseGroupPermissionStatus of
        ValidReleaseGroupPermission -> pure ()
        InvalidEditReleaseGroupPermission -> fatalText "You do not have permission to edit this release group"
        InvalidCreateTeamProjectsForReleaseGroupPermission -> fatalText "You do not have permission to add projects to all teams which include this release group"
    Nothing ->
      case projectPermissionStatus of
        ValidProjectPermission -> pure ()
        InvalidEditProjectPermission -> fatalText "You do not have permission to edit this project"
        InvalidCreateProjectPermission -> fatalText "You do not have permission to create project"
        InvalidCreateTeamProjectPermission -> fatalText "You do not have permission to a project for the specified team"
        InvalidCreateProjectOnlyToTeamPermission -> fatalText "You permissions create projects but only for a specified team"

fullAccessTokenCheck :: Has Diagnostics sig m => TokenTypeResponse -> m ()
fullAccessTokenCheck TokenTypeResponse{..} = case tokenType of
  Push -> fatalText "You are using a push-only token when you need a full-access token"
  _ -> pure ()

premiumSubscriptionCheck :: Has Diagnostics sig m => SubscriptionResponse -> m ()
premiumSubscriptionCheck SubscriptionResponse{..} = case subscription of
  Free -> fatalText "You have free subscription when you need premium subscription"
  _ -> pure ()

preflightCheckFileName :: Path Rel File
preflightCheckFileName = $(mkRelFile "preflight-check.txt")

data InvalidApiKeyErr = InvalidApiKeyErr
instance ToDiagnostic InvalidApiKeyErr where
  renderDiagnostic InvalidApiKeyErr =
    vsep
      [ "Ensure that you are using a valid FOSSA_API_KEY."
      , "Refer to " <> pretty apiKeyUrl <> " for guidance on how to generate and retrieve your API key."
      ]
