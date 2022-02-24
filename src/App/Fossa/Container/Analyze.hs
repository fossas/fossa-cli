{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Container.Analyze (
  analyze,
  containerScanningDocUrl,
) where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Config.Common (ScanDestination (..))
import App.Fossa.Config.Container (
  ContainerAnalyzeConfig (
    ContainerAnalyzeConfig,
    imageLocator,
    revisionOverride,
    scanDestination
  ),
 )
import App.Fossa.Config.Container.Common (ImageText (ImageText))
import App.Fossa.Container.Scan (extractRevision, runSyft, toContainerScan)
import App.Fossa.FossaAPIV1 (UploadResponse (uploadError, uploadLocator), uploadContainerScan)
import App.Types (ProjectRevision (..))
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, errCtx, renderDiagnostic)
import Control.Effect.Lift (Lift)
import Data.Aeson (encode)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import Effect.Logger (
  AnsiStyle,
  Has,
  Logger,
  Pretty (pretty),
  logDebug,
  logError,
  logInfo,
  logStdout,
  viaShow,
 )
import Prettyprinter (Doc, indent, vsep)
import Srclib.Types (parseLocator)

analyze ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  ContainerAnalyzeConfig ->
  m ()
analyze ContainerAnalyzeConfig{..} = do
  logDebug "Running embedded syft binary"
  containerScan <- errCtx (SyftScanFailed imageLocator) (runSyft imageLocator >>= toContainerScan)
  case scanDestination of
    OutputStdout -> logStdout . decodeUtf8 $ encode containerScan
    UploadScan apiOpts projectMeta -> do
      let revision = extractRevision revisionOverride containerScan
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

newtype SyftScanFailed = SyftScanFailed ImageText

instance ToDiagnostic SyftScanFailed where
  renderDiagnostic (SyftScanFailed (ImageText img)) =
    vsep
      [ pretty $ "Failed to analyze container for: " <> img
      , ""
      , "Please ensure you are providing image in supported formats (e.g. docker container analyze repo/image:tag):"
      , indent 2 exampleSupportedFmt
      , ""
      , "Refer to:"
      , indent 2 $ pretty ("- " <> containerScanningDocUrl)
      ]
    where
      exampleSupportedFmt :: Doc AnsiStyle
      exampleSupportedFmt =
        vsep
          [ "repo/image:tag           - image from docker daemon"
          , "path/to/image.tar        - tarball from disk created with `docker image save, podman save, skopeo copy, etc.`"
          , "registry:repo/image:tag  - pull image directly from a registry (no container runtime required)"
          , "03xxx29dad99             - docker Image ID provided in `docker images`"
          ]

containerScanningDocUrl :: Text
containerScanningDocUrl = "https://docs.fossa.com/docs/container-scanning#using-container-scanning"
