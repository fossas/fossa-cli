{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Container.AnalyzeNative (
  analyzeExperimental,
) where

import App.Fossa.Config.Container.Analyze (
  ContainerAnalyzeConfig (
    ContainerAnalyzeConfig,
    imageLocator,
    revisionOverride,
    scanDestination
  ),
 )
import App.Fossa.Config.Container.Common (ImageText (unImageText))
import Control.Effect.Diagnostics (Diagnostics, fatalText, fromEitherShow)
import Control.Effect.Lift (Lift)
import Data.String.Conversion (toString)
import Data.Text (Text)
import Effect.Logger (
  Has,
  Logger,
  logInfo,
 )
import Effect.ReadFS (ReadFS, getCurrentDir)
import Path (Abs, File, Path, SomeBase (Abs, Rel), parseSomeFile, (</>))

data ContainerImageSource
  = ContainerExportedTarball (Path Abs File)
  | ContainerDockerImage Text
  | ContainerOCIRegistry Text Text
  deriving (Show, Eq)

analyzeExperimental ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  ContainerAnalyzeConfig ->
  m ()
analyzeExperimental ContainerAnalyzeConfig{..} = do
  logInfo "Running container scanning with fossa experimental-scanner!"
  parsedSource <- parseContainerImageSource (unImageText imageLocator)
  case parsedSource of
    ContainerDockerImage _ -> fatalText "container images from daemon are not yet supported!"
    ContainerOCIRegistry _ _ -> fatalText "container images from oci registry are not yet supported!"
    ContainerExportedTarball _ -> fatalText "exported container images are not yet supported!"

parseContainerImageSource ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Text ->
  m (ContainerImageSource)
parseContainerImageSource = parseExportedTarballSource

parseExportedTarballSource ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Text ->
  m (ContainerImageSource)
parseExportedTarballSource path = do
  cwd <- getCurrentDir
  someTarballFile <- fromEitherShow $ parseSomeFile (toString path)
  resolvedAbsPath <- case someTarballFile of
    Abs absPath -> pure absPath
    Rel relPath -> pure $ cwd </> relPath
  pure $ ContainerExportedTarball resolvedAbsPath
