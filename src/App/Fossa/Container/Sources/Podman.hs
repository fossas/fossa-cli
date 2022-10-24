{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Container.Sources.Podman (
  analyzeFromPodman,
  podmanInspectImage,
  listTargetsFromPodman,
  revisionFromPodman,
) where

import App.Fossa.Container.Sources.DockerArchive (analyzeFromDockerArchive, listTargetsFromDockerArchive, revisionFromDockerArchive)
import Container.Types (ContainerScan)
import Control.Carrier.Lift (Lift)
import Control.Effect.Debug (Debug, Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Path (withSystemTempDir)
import Control.Effect.Telemetry (Telemetry)
import Control.Monad (void)
import Data.String.Conversion (ToText (toText), toString)
import Data.Text (Text)
import Discovery.Filters (AllFilters)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execThrow')
import Effect.Logger (Logger, logInfo, pretty)
import Effect.ReadFS (ReadFS)
import Path (Abs, File, Path, mkRelFile, toFilePath, (</>))

-- | Inspects a image, if image does not exist throws an error.
-- Refer to: https://docs.podman.io/en/latest/markdown/podman-image-inspect.1.html
podmanInspectImage :: Text -> Command
podmanInspectImage img =
  Command
    { cmdName = "podman"
    , cmdArgs = ["image", "inspect", img]
    , cmdAllowErr = Never
    }

-- | Saves container image to a location in docker archive (tarball) format.
-- Refer to: https://docs.podman.io/en/latest/markdown/podman-save.1.html
podmanExtractImage :: Text -> Path Abs File -> Command
podmanExtractImage img dest =
  Command
    { cmdName = "podman"
    , cmdArgs = ["save", "--format", "docker-archive", img, "-o", toText . toFilePath $ dest]
    , cmdAllowErr = Never
    }

runFromPodman ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Text ->
  (Path Abs File -> m b) ->
  m b
runFromPodman img f = do
  withSystemTempDir "fossa-podman-tmp" $ \dir -> do
    let tempTarFile = dir </> $(mkRelFile "image.tar")
    logInfo . pretty $
      "Exporting image to temp file: "
        <> toString tempTarFile
        <> "! This may take a while!"
    void $ execThrow' (podmanExtractImage img tempTarFile)
    logInfo . pretty $ "Analyzing exported container image: " <> toString tempTarFile
    f tempTarFile

analyzeFromPodman ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Telemetry sig m
  , Has Debug sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Bool ->
  AllFilters ->
  Text ->
  m ContainerScan
analyzeFromPodman systemDepsOnly filters img = runFromPodman img $ analyzeFromDockerArchive systemDepsOnly filters

listTargetsFromPodman ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has Telemetry sig m
  ) =>
  Text ->
  m ()
listTargetsFromPodman img = runFromPodman img listTargetsFromDockerArchive

revisionFromPodman ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Text ->
  m (Text, Text)
revisionFromPodman img = runFromPodman img revisionFromDockerArchive
