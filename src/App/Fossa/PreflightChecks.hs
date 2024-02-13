{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.PreflightChecks (
  PreflightCommandChecks (..),
  preflightChecks,
  guardWithPreflightChecks,
) where

import App.Docs (apiKeyUrl)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics (Diagnostics, errCtx)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.Stack (context)
import Control.Effect.Diagnostics (ToDiagnostic, fatalOnIOException, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, FossaApiClientF (AssertUserDefinedBinaries), getOrganization, getSubscription, getTokenType)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (void)
import Data.Text.IO qualified as TIO
import Diag.Diagnostic (ToDiagnostic (..))
import Effect.Logger (pretty, vsep)
import Fossa.API.Types (ApiOpts, Subscription (..), TokenType (..))
import Path (
  File,
  Path,
  Rel,
  fromAbsFile,
  mkRelFile,
  (</>),
 )
import Path.IO (getTempDir, removeFile)

data PreflightCommandChecks = AnalyzeChecks | TestChecks | ReportChecks | ConatinerAnalzyeChecks | ContainerTestChecks | AssertUserDefinedBinariesChecks

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
  tokenType <- errCtx InvalidApiKeyErr getTokenType
  subscription <- getSubscription

  void $ case cmd of
    TestChecks -> do
      fullAccessTokenCheck tokenType
    _ -> do
      fullAccessTokenCheck tokenType
      premiumSubscriptionCheck subscription

  void $ errCtx InvalidApiKeyErr getOrganization

fullAccessTokenCheck :: Has Diagnostics sig m => TokenType -> m ()
fullAccessTokenCheck token = case token of
  Push -> fatalText "You are using a push-only token when you need a full-access token"
  _ -> pure ()

premiumSubscriptionCheck :: Has Diagnostics sig m => Subscription -> m ()
premiumSubscriptionCheck subscription = case subscription of
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
