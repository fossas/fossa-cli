module Container.Errors (ContainerImgParsingError(..)) where

import qualified Codec.Archive.Tar as Tar
import Control.Exception (Exception)
import Control.DeepSeq (NFData (rnf))

-- | Errors that can be encountered when parsing a container image.
data ContainerImgParsingError
  = ManifestFileMissing
  | ManifestJsonNotFound
  | ManifestJsonParsingFailed String
  | TarParserError Tar.FormatError
  | TarMissingLayerTar FilePath
  deriving (Eq)

instance Show ContainerImgParsingError where
  show ManifestFileMissing = "ManifestFileMissing - could not find manifest.json in tarball."
  show ManifestJsonNotFound = "ManifestJsonNotFound - could not find manifest.json in tarball."
  show (ManifestJsonParsingFailed err) = "ManifestJsonParsingFailed: " <> show err
  show (TarParserError err) = "TarParserError: " <> show err
  show (TarMissingLayerTar path)= "TarMissingLayerTar: " <> show path

instance Exception ContainerImgParsingError

instance NFData    ContainerImgParsingError where
  rnf !_ = () -- enumerations are fully strict by construction