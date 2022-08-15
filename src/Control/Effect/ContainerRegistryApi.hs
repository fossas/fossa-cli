{-# LANGUAGE GADTs #-}

module Control.Effect.ContainerRegistryApi (
  ContainerRegistryApi,
  ContainerRegistryApiF (..),
  getImageManifest,
  exportImage,
) where

import Container.Docker.OciManifest (OciManifestV2)
import Container.Docker.SourceParser (RegistryImageSource)
import Control.Algebra (Has)
import Control.Carrier.Simple (Simple, sendSimple)
import Path (Abs, Dir, File, Path)

type ContainerRegistryApi = Simple ContainerRegistryApiF

data ContainerRegistryApiF a where
  GetImageManifest :: RegistryImageSource -> ContainerRegistryApiF (OciManifestV2)
  ExportImage :: RegistryImageSource -> Path Abs Dir -> ContainerRegistryApiF (Path Abs File)

-- | Retrieves Image Manifest from Container Registry.
getImageManifest ::
  Has ContainerRegistryApi sig m =>
  RegistryImageSource ->
  m OciManifestV2
getImageManifest = sendSimple . GetImageManifest

-- | Exports Image in (tarball format) from Registry.
exportImage ::
  Has ContainerRegistryApi sig m =>
  RegistryImageSource ->
  Path Abs Dir -> -- Where to export image.
  m (Path Abs File) -- Absolute path to Exported Image.
exportImage registrySource dir = sendSimple $ ExportImage registrySource dir
