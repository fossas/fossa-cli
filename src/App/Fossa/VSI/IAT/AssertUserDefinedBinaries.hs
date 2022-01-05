{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSI.IAT.AssertUserDefinedBinaries (
  linkBinsSubCommand,
) where

import App.Fossa.Config.LinkUserBinaries (
  LinkUserBinsConfig (..),
  LinkUserBinsOpts,
  mkSubCommand,
 )
import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.Subcommand (SubCommand)
import App.Fossa.VSI.Fingerprint (fingerprintContentsRaw)
import App.Types (BaseDir (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
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
  logInfo "Fingerprinting directory contents"
  fingerprints <- fingerprintContentsRaw $ unBaseDir baseDir

  logInfo "Uploading assertion to FOSSA"
  Fossa.assertUserDefinedBinaries apiOpts binMetadata fingerprints
