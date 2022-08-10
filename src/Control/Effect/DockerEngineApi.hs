{-# LANGUAGE GADTs #-}

module Control.Effect.DockerEngineApi (
  DockerEngineApiF (..),
  DockerEngineApi,
  getDockerImage,
  getDockerImageSize,
  isDockerEngineAccessible,
) where

import Control.Carrier.Simple (Has, Simple, sendSimple)
import Data.Text (Text)
import Path (Abs, File, Path)

data DockerEngineApiF a where
  ExportImage :: Text -> Path Abs File -> DockerEngineApiF ()
  GetImageSize :: Text -> DockerEngineApiF Int
  IsDockerEngineAccessible :: DockerEngineApiF Bool

type DockerEngineApi = Simple DockerEngineApiF

deriving instance Show (DockerEngineApiF a)
deriving instance Eq (DockerEngineApiF a)

-- | Exports Docker Image Tarball to given path.
getDockerImage :: (Has DockerEngineApi sig m) => Text -> Path Abs File -> m ()
getDockerImage img path = sendSimple (ExportImage img path)

-- | Gets Docker Image Size in Bytes.
getDockerImageSize :: (Has DockerEngineApi sig m) => Text -> m Int
getDockerImageSize = sendSimple . GetImageSize

-- | True if Docker Engine is Accessible
isDockerEngineAccessible :: (Has DockerEngineApi sig m) => m Bool
isDockerEngineAccessible = sendSimple IsDockerEngineAccessible
