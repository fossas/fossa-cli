{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.PreflightChecks (
  preflightChecks,
) where

import App.Docs (apiKeyUrl)
import Control.Carrier.Diagnostics (Diagnostics, errCtx, fatalText)
import Control.Carrier.Stack (context)
import Control.Effect.Diagnostics (ToDiagnostic, fatal, fatalOnIOException)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization, getTokenType, getSubscription)
import Control.Effect.Lift
import Control.Monad (when)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Diag.Diagnostic (ToDiagnostic (..))
import Effect.Logger (Logger, logDebug, pretty, vsep)
import Fossa.API.Types (TokenType (..), Subscription (..))
import Path (
  File,
  Path,
  Rel,
  fromAbsFile,
  mkRelFile,
  (</>),
 )
import Path.IO

preflightChecks ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has FossaApiClient sig m
  , Has Logger sig m
  ) =>
  m ()
preflightChecks = context "preflight-checks" $ do
  logDebug "**************** %%%%%%%%%%%%% In Preflight Checks "
  -- Check for writing to temp dir
  tmpDir <- sendIO getTempDir
  fatalOnIOException "Failed to write to temp directory" . sendIO $ TIO.writeFile (fromAbsFile $ tmpDir </> preflightCheckFileName) "Writing to temp dir"
  sendIO $ removeFile (tmpDir </> preflightCheckFileName)

  -- Check for valid API Key and if user can connect to fossa app
  -- _ <- errCtx InvalidApiKeyErr getOrganization

  -- pushTokenRes <- pushTokenCheck
  -- logDebug $ "Push Token Response ------------ " <> pretty (show pushTokenRes)
  -- when (isPushToken pushTokenRes) $ fatalText "You are using a push-only token when you need a full-access token"

preflightCheckFileName :: Path Rel File
preflightCheckFileName = $(mkRelFile "preflight-check.txt")

data InvalidApiKeyErr = InvalidApiKeyErr
instance ToDiagnostic InvalidApiKeyErr where
  renderDiagnostic InvalidApiKeyErr =
    vsep
      [ "Ensure that you are using a valid FOSSA_API_KEY."
      , "Refer to " <> pretty apiKeyUrl <> " for guidance on how to generate and retrieve your API key."
      ]
