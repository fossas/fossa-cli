module App.Fossa.Container.ListTargets (listTargets) where

import App.Fossa.Config.Container.Analyze (
  ContainerAnalyzeConfig (arch, imageLocator),
  dockerHost,
 )
import App.Fossa.Config.Container.Common (ImageText (unImageText))
import App.Fossa.Container.AnalyzeNative (
  ContainerImageSource (..),
  parseContainerImageSource,
 )
import App.Fossa.Container.Sources.DockerArchive (listTargetsFromDockerArchive)
import App.Fossa.Container.Sources.DockerEngine (listTargetsFromDockerEngine)
import App.Fossa.Container.Sources.Podman (listTargetsFromPodman)
import App.Fossa.Container.Sources.Registry (listTargetsFromRegistry)
import Control.Carrier.AtomicCounter (
  Has,
 )
import Control.Carrier.DockerEngineApi (runDockerEngineApi)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Control.Effect.Stack (Stack)
import Effect.Exec (Exec)
import Effect.Logger (
  Logger,
 )
import Effect.ReadFS (ReadFS)

listTargets ::
  ( Has Logger sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Stack sig m
  , Has Diagnostics sig m
  ) =>
  ContainerAnalyzeConfig ->
  m ()
listTargets cfg = do
  parsedSource <- runDockerEngineApi (dockerHost cfg) $ parseContainerImageSource (unImageText $ imageLocator cfg) (arch cfg)
  case parsedSource of
    DockerArchive tarball -> listTargetsFromDockerArchive tarball
    DockerEngine imgTag -> listTargetsFromDockerEngine (dockerHost cfg) imgTag
    Podman img -> listTargetsFromPodman img
    Registry registrySrc -> listTargetsFromRegistry registrySrc
