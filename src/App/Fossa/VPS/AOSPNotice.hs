module App.Fossa.VPS.AOSPNotice
  ( aospNoticeMain,
  ) where

import Control.Effect.Lift (sendIO, Lift)
import Control.Carrier.Diagnostics
import Effect.Exec
import System.Exit (exitFailure)

import App.Fossa.EmbeddedBinary
import App.Fossa.VPS.Scan.RunWiggins
import App.Fossa.VPS.Types
import App.Types (BaseDir (..), OverrideProject)
import Effect.Logger
import Data.Text (Text)
import App.Fossa.ProjectInference
import Fossa.API.Types (ApiOpts(..))
import Path (Path, Abs, Dir)

aospNoticeMain :: BaseDir -> Severity -> OverrideProject -> NinjaScanID -> NinjaFilePaths -> ApiOpts -> IO ()
aospNoticeMain (BaseDir basedir) logSeverity overrideProject ninjaScanId ninjaFilePaths apiOpts = withLogger logSeverity $ do
  result <- runDiagnostics $ withWigginsBinary $ aospNoticeGenerate basedir logSeverity overrideProject ninjaScanId ninjaFilePaths apiOpts
  case result of
    Left failure -> do
      logError $ renderFailureBundle failure
      sendIO exitFailure
    Right bundle -> logWarn (renderWarnings $ resultWarnings bundle)

----- main logic

aospNoticeGenerate ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  ) => Path Abs Dir -> Severity -> OverrideProject -> NinjaScanID -> NinjaFilePaths -> ApiOpts -> BinaryPaths -> m ()
aospNoticeGenerate basedir logSeverity overrideProject ninjaScanId ninjaFilePaths apiOpts binaryPaths = do
  projectRevision <- mergeOverride overrideProject <$> (inferProjectFromVCS basedir <||> inferProjectDefault basedir)

  let wigginsOpts = generateWigginsAOSPNoticeOpts basedir logSeverity apiOpts projectRevision ninjaScanId ninjaFilePaths

  logInfo "Running VPS plugin: generating AOSP notice files"
  stdout <- runExecIO $ runWiggins binaryPaths wigginsOpts
  logInfo $ pretty stdout

runWiggins :: ( Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m Text
runWiggins binaryPaths opts = do
  execWiggins binaryPaths opts
