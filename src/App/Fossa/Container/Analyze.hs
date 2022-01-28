module App.Fossa.Container.Analyze (
  analyzeMain,
) where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Analyze (ScanDestination (..))
import App.Fossa.Container (ImageText (..), extractRevision, runSyft, toContainerScan)
import App.Fossa.FossaAPIV1 (UploadResponse (uploadError, uploadLocator), uploadContainerScan)
import App.Types (OverrideProject (..), ProjectRevision (..))
import Control.Carrier.Diagnostics (Diagnostics, logWithExit_)
import Control.Carrier.Stack (runStack)
import Control.Effect.Lift (Lift)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (encode)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (decodeUtf8)
import Effect.Logger
import Srclib.Types (parseLocator)

analyzeMain :: ScanDestination -> Severity -> OverrideProject -> ImageText -> IO ()
analyzeMain scanDestination logSeverity override image = runStack . withDefaultLogger logSeverity . logWithExit_ $ analyze scanDestination override image

analyze ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , MonadIO m
  ) =>
  ScanDestination ->
  OverrideProject ->
  ImageText ->
  m ()
analyze scanDestination override image = do
  logDebug "Running embedded syft binary"
  containerScan <- runSyft image >>= toContainerScan
  case scanDestination of
    OutputStdout -> logStdout . decodeUtf8 $ encode containerScan
    UploadScan apiOpts projectMeta -> do
      let revision = extractRevision override containerScan
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using project revision: `" <> pretty (projectRevision revision) <> "`")
      let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
      logInfo ("Using branch: `" <> pretty branchText <> "`")

      resp <- uploadContainerScan apiOpts revision projectMeta containerScan

      buildUrl <- getFossaBuildUrl revision apiOpts . parseLocator $ uploadLocator resp
      logInfo "View FOSSA Report:"
      logInfo ("  " <> pretty buildUrl)
      -- Report non-critical errors
      traverse_ (\err -> logError $ "FOSSA error: " <> viaShow err) (uploadError resp)
