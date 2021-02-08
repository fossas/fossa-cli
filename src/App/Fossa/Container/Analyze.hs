
module App.Fossa.Container.Analyze
  ( analyzeMain,
  )
where

import App.Fossa.Analyze (ScanDestination (..))
import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Container (ImageText (..), runSyft, toContainerScan, extractRevision)
import App.Fossa.FossaAPIV1 (uploadContainerScan)
import App.Types (OverrideProject (..), ProjectRevision (..))
import Control.Carrier.Diagnostics
import Control.Effect.Lift (Lift)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Text.Lazy.Encoding (decodeUtf8)
import Effect.Logger
import Srclib.Types (parseLocator)

analyzeMain :: ScanDestination -> Severity -> OverrideProject -> ImageText -> IO ()
analyzeMain scanDestination logSeverity override image = withLogger logSeverity $ do
  result <- runDiagnostics $ analyze scanDestination override image
  case result of
    Left err -> logError (renderFailureBundle err)
    Right _ -> pure ()

analyze ::
  ( Has Diagnostics sig m,
    Has (Lift IO) sig m,
    Has Logger sig m,
    MonadIO m
  ) =>
  ScanDestination ->
  OverrideProject ->
  ImageText ->
  m ()
analyze scanDestination override image = do
  logDebug "Running embedded syft binary"
  containerScan <- runSyft image >>= toContainerScan
  case scanDestination of
    OutputStdout -> logStdout . pretty . decodeUtf8 $ encode containerScan
    UploadScan apiOpts projectMeta -> do
      let revision = extractRevision override containerScan
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using project revision: `" <> pretty (projectRevision revision) <> "`")
      locator <- uploadContainerScan apiOpts projectMeta containerScan
      logInfo "Container Analysis successfully uploaded!"
      buildUrl <- getFossaBuildUrl revision apiOpts $ parseLocator locator
      logInfo "View FOSSA Report:"
      logInfo ("  " <> pretty buildUrl)
      pure ()

