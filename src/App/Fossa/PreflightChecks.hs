{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.PreflightChecks (
  preflightChecks,
  guardWithPreflightChecks,
) where

import App.Docs (apiKeyUrl)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics (Diagnostics, errDoc, errHelp)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.Stack (context)
import Control.Effect.Diagnostics (ToDiagnostic, fatalOnIOException)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (void)
import Data.Error (createErrataWithHeaderOnly)
import Data.Text.IO qualified as TIO
import Diag.Diagnostic (ToDiagnostic (..))
import Errata (Errata)
import Fossa.API.Types (ApiOpts)
import Path (
  File,
  Path,
  Rel,
  fromAbsFile,
  mkRelFile,
  (</>),
 )
import Path.IO (getTempDir, removeFile)

guardWithPreflightChecks ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  ApiOpts ->
  m ()
guardWithPreflightChecks apiOpts = ignoreDebug $ runFossaApiClient apiOpts preflightChecks

preflightChecks ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has FossaApiClient sig m
  ) =>
  m ()
preflightChecks = context "preflight-checks" $ do
  -- Check for writing to temp dir
  tmpDir <- sendIO getTempDir
  fatalOnIOException "Failed to write to temp directory" . sendIO $ TIO.writeFile (fromAbsFile $ tmpDir </> preflightCheckFileName) "Writing to temp dir"
  sendIO $ removeFile (tmpDir </> preflightCheckFileName)

  -- Check for valid API Key and if user can connect to fossa app
  void $ errHelp InvalidApiKeyErr $ errDoc apiKeyUrl getOrganization

preflightCheckFileName :: Path Rel File
preflightCheckFileName = $(mkRelFile "preflight-check.txt")

data InvalidApiKeyErr = InvalidApiKeyErr
instance ToDiagnostic InvalidApiKeyErr where
  renderDiagnostic :: InvalidApiKeyErr -> Errata
  renderDiagnostic InvalidApiKeyErr =
    createErrataWithHeaderOnly "Ensure that you are using a valid FOSSA_API_KEY. Refer to the provided documentation for guidance on how to generate and retrieve your API key."
