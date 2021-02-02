module App.Fossa.VPS.AOSPNotice
  ( aospNoticeMain,
    WriteEnabled(..)
  ) where

import Control.Effect.Lift (sendIO, Lift)
import Control.Carrier.Diagnostics
import Effect.Exec
import System.Exit (exitFailure)

import App.Fossa.EmbeddedBinary
import App.Fossa.VPS.Scan.RunWiggins
import App.Fossa.VPS.Types
import App.Types (BaseDir (..))
import Data.Flag (Flag, fromFlag)
import Effect.Logger
import Data.Text (Text)

-- | WriteEnabled bool flag
data WriteEnabled = WriteEnabled

aospNoticeMain :: BaseDir -> Severity -> FilterExpressions -> Flag WriteEnabled ->  IO ()
aospNoticeMain basedir logSeverity fileFilters writeEnabled = withLogger logSeverity $ do
  result <- runDiagnostics $ withWigginsBinary $ aospNoticeGenerate basedir logSeverity writeEnabled fileFilters
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
  ) => BaseDir -> Severity -> Flag WriteEnabled -> FilterExpressions -> BinaryPaths -> m ()
aospNoticeGenerate (BaseDir basedir) logSeverity writeEnabled fileFilters binaryPaths =  do
  let wigginsOpts = generateWigginsAOSPNoticeOpts basedir logSeverity fileFilters (fromFlag WriteEnabled writeEnabled)

  logInfo "Running VPS plugin: generating AOSP notice files"
  stdout <- runExecIO $ runWiggins binaryPaths wigginsOpts
  logInfo $ pretty stdout

runWiggins :: ( Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m Text
runWiggins binaryPaths opts = do
  execWiggins binaryPaths opts