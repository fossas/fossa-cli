{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.PreflightChecks (
  preflightChecks,
) where

import App.Docs (mainReadMeUrl)
import Control.Carrier.Diagnostics (Diagnostics, errCtx)
import Control.Carrier.Stack (context)
import Control.Effect.Diagnostics (ToDiagnostic, fatalOnIOException)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
import Control.Effect.Lift
import Data.Text.IO qualified as TIO
import Diag.Diagnostic (ToDiagnostic (..))
import Effect.Logger (pretty, vsep)
import Path.IO (getTempDir, removeFile)
import Path.Posix

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
  _ <- errCtx InvalidApiKeyErr getOrganization
  pure ()

preflightCheckFileName :: Path Rel File
preflightCheckFileName = $(mkRelFile "preflight-check.txt")

data InvalidApiKeyErr = InvalidApiKeyErr
instance ToDiagnostic InvalidApiKeyErr where
  renderDiagnostic InvalidApiKeyErr =
    vsep
      [ "Ensure that you are using a valid FOSSA_API_KEY."
      , "Refer to " <> pretty mainReadMeUrl <> " for guidance on how to generate and retrieve your API key."
      ]