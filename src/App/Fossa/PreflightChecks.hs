{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.PreflightChecks (
    preflightChecks,
) where

import Control.Carrier.Diagnostics (Diagnostics)
import Control.Carrier.Stack (context)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
import Control.Effect.Lift
import Data.Text.IO qualified as TIO
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
    sendIO $ TIO.writeFile (fromAbsFile $ tmpDir </> preflightCheckFileName) "Writing to temp dir"
    sendIO $ removeFile (tmpDir </> preflightCheckFileName)

    -- Check for valid API Key and if user can connect to fossa app
    _ <- getOrganization
    pure ()

preflightCheckFileName :: Path Rel File
preflightCheckFileName = $(mkRelFile "preflight-check.txt")