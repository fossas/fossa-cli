module Container.Errors (
  ContainerImgParsingError (..),
  EndpointDoesNotSupportNativeContainerScan (..),
) where

import Codec.Archive.Tar qualified as Tar
import Control.Exception (Exception)
import Data.List.NonEmpty (NonEmpty)
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Effect.Logger (pretty)
import Prettyprinter (vsep)

-- | Errors that can be encountered when parsing a container image.
data ContainerImgParsingError
  = ManifestFileMissing
  | ManifestJsonNotFound
  | ManifestJsonParsingFailed String
  | TarParserError Tar.FormatError
  | TarMissingLayerTar FilePath
  | TarLayerNotAFile FilePath
  | TarballFileNotFound FilePath
  | ContainerNoLayersDiscovered
  deriving (Eq)

instance Show ContainerImgParsingError where
  show ManifestFileMissing = "ManifestFileMissing - could not find manifest.json in tarball."
  show ManifestJsonNotFound = "ManifestJsonNotFound - could not find manifest.json in tarball."
  show (ManifestJsonParsingFailed err) = "ManifestJsonParsingFailed: " <> show err
  show (TarParserError err) = "TarParserError: " <> show err
  show (TarMissingLayerTar path) = "TarMissingLayerTar: " <> show path
  show (TarLayerNotAFile path) = "TarLayerNotAFile: " <> show path
  show (TarballFileNotFound path) = "TarballFileNotFound: " <> show path
  show (ContainerNoLayersDiscovered) = "ContainerNoLayersDiscovered - could not analyze or detect any layer."

instance Exception ContainerImgParsingError

instance ToDiagnostic ContainerImgParsingError where
  renderDiagnostic = pretty . show

instance ToDiagnostic (NonEmpty ContainerImgParsingError) where
  renderDiagnostic = pretty . show

data EndpointDoesNotSupportNativeContainerScan = EndpointDoesNotSupportNativeContainerScan
instance ToDiagnostic EndpointDoesNotSupportNativeContainerScan where
  renderDiagnostic (EndpointDoesNotSupportNativeContainerScan) =
    vsep
      [ "Provided endpoint does not support native container scans."
      , ""
      , "If you are using, --experimental-scanner option, it is not supported for your"
      , "FOSSA instance. Try without using --experimental-scanner."
      , ""
      , "If your instance of FOSSA is on-premise, it likely needs to be updated to latest version."
      , "Please contact FOSSA support for more assistance."
      ]
