module App.Fossa.Container.ListTargets (listTargets) where

import App.Fossa.Config.Container.Common (ImageText (unImageText))
import App.Fossa.Config.Container.ListTargets (ContainerListTargetsConfig (arch, cfgImageLocator, dockerHost))
import App.Fossa.Container.Scan (ContainerImageSource (..), parseContainerImageSource)
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
import Control.Effect.Telemetry (Telemetry)
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
  , Has Telemetry sig m
  ) =>
  ContainerListTargetsConfig ->
  m ()
listTargets cfg = do
  parsedSource <-
    runDockerEngineApi (dockerHost cfg) $
      parseContainerImageSource
        (unImageText $ cfgImageLocator cfg)
        (arch cfg)

  case parsedSource of
    DockerArchive tarball -> listTargetsFromDockerArchive tarball
    DockerEngine imgTag -> listTargetsFromDockerEngine (dockerHost cfg) imgTag
    Podman img -> listTargetsFromPodman img
    Registry registrySrc -> listTargetsFromRegistry registrySrc
