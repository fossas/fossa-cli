{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSI.IAT.AssertUserDefinedBinaries (
  linkBinsSubCommand,
) where

import App.Fossa.Config.LinkUserBinaries (
  LinkUserBinsConfig (..),
  LinkUserBinsOpts,
  mkSubCommand,
 )
import App.Fossa.PreflightChecks (PreflightCommandChecks (AssertUserDefinedBinariesChecks), guardWithPreflightChecks)
import App.Fossa.Subcommand (SubCommand)
import App.Fossa.VSI.Fingerprint (fingerprintContentsRaw)
import App.Fossa.VSIDeps (userEnabledMsb, userEnabledMsbErrorMsg)
import App.Types (BaseDir (..))
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.FossaApiClient qualified as API
import Control.Effect.Lift (Lift)
import Control.Monad (unless, void)
import Effect.Logger (Logger, logInfo)
import Effect.ReadFS (ReadFS)

linkBinsSubCommand :: SubCommand LinkUserBinsOpts LinkUserBinsConfig
linkBinsSubCommand = mkSubCommand assertUserDefinedBinaries

assertUserDefinedBinaries ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  LinkUserBinsConfig ->
  m ()
assertUserDefinedBinaries LinkUserBinsConfig{..} = do
  msbEnabled <- userEnabledMsb
  unless msbEnabled $ fatalText userEnabledMsbErrorMsg

  void $ guardWithPreflightChecks apiOpts AssertUserDefinedBinariesChecks

  logInfo "Fingerprinting directory contents"
  fingerprints <- fingerprintContentsRaw $ unBaseDir baseDir

  logInfo "Uploading assertion to FOSSA"
  ignoreDebug . runFossaApiClient apiOpts $ API.assertUserDefinedBinaries binMetadata fingerprints
