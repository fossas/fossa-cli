{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.AOSPNotice (
  aospNoticeMain,
) where

import App.Fossa.EmbeddedBinary (BinaryPaths, withWigginsBinary)
import App.Fossa.VPS.Scan.RunWiggins (
  WigginsOpts,
  execWiggins,
  generateWigginsAOSPNoticeOpts,
 )
import App.NewFossa.Config.VPS (AOSPNoticeConfig (..))
import App.Types (BaseDir (..))
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Lift (Lift)
import Data.Text (Text)
import Effect.Exec (Exec)
import Effect.Logger (Logger, Pretty (pretty), logInfo)

aospNoticeMain ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  ) =>
  AOSPNoticeConfig ->
  m ()
aospNoticeMain AOSPNoticeConfig{..} = withWigginsBinary $ \binaryPaths -> do
  let wigginsOpts =
        generateWigginsAOSPNoticeOpts
          (unBaseDir aospBaseDir)
          aospSeverity
          aospApiOpts
          aospRevision
          ninjaScanId
          ninjaFileList

  logInfo "Running VPS plugin: generating AOSP notice files"
  stdout <- runWiggins binaryPaths wigginsOpts
  logInfo $ pretty stdout

runWiggins :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m Text
runWiggins binaryPaths opts = do
  execWiggins binaryPaths opts
