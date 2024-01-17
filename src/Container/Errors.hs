module Container.Errors (
  ContainerImgParsingError (..),
  EndpointDoesNotSupportNativeContainerScan (..),
) where

import App.Support (supportUrl)
import Codec.Archive.Tar qualified as Tar
import Control.Exception (Exception)
import Data.Error (SourceLocation, createBlock)
import Data.List.NonEmpty (NonEmpty)
import Data.String.Conversion (toText)
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Effect.Logger (pretty, renderIt)
import Errata (Errata (..), errataSimple)
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
  renderDiagnostic e = Errata (Just (toText $ show e)) [] Nothing

instance ToDiagnostic (NonEmpty ContainerImgParsingError) where
  renderDiagnostic e = Errata (Just (toText $ show e)) [] Nothing

newtype EndpointDoesNotSupportNativeContainerScan = EndpointDoesNotSupportNativeContainerScan SourceLocation
instance ToDiagnostic EndpointDoesNotSupportNativeContainerScan where
  renderDiagnostic (EndpointDoesNotSupportNativeContainerScan srcLoc) = do
    let header = "Provided endpoint does not support native container scans"
        body =
          renderIt $
            vsep
              [ "Container scanning with new scanner is not supported for your FOSSA endpoint."
              , ""
              , "Upgrade your FOSSA instance to v4.0.37 or downgrade your FOSSA CLI to 3.4.x"
              , ""
              , "Please contact FOSSA support at " <> pretty supportUrl <> " for more assistance."
              ]
        block = createBlock srcLoc Nothing Nothing
    errataSimple (Just header) block (Just body)
