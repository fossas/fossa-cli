module Strategy.Swift.Errors (
  MissingPackageResolvedFile (..),

  -- * docs
  swiftFossaDocUrl,
  swiftPackageResolvedRef,
  xcodeCoordinatePkgVersion,
) where

import App.Docs (platformDocUrl)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Diag.Diagnostic (DiagnosticInfo (..), ToDiagnostic, renderDiagnostic)
import Path
import Prettyprinter (Pretty (pretty), indent, viaShow, vsep)

swiftFossaDocUrl :: Text
swiftFossaDocUrl = platformDocUrl "ios/swift.md"

swiftPackageResolvedRef :: Text
swiftPackageResolvedRef = "https://github.com/apple/swift-package-manager/blob/main/Documentation/Usage.md#resolving-versions-packageresolved-file"

xcodeCoordinatePkgVersion :: Text
xcodeCoordinatePkgVersion = "https://developer.apple.com/documentation/swift_packages/adding_package_dependencies_to_your_app"

newtype MissingPackageResolvedFile = MissingPackageResolvedFile (Path Abs File)

instance ToDiagnostic MissingPackageResolvedFile where
  renderDiagnostic (MissingPackageResolvedFile path) = do
    let ctx = "We could not perform Package.resolved analysis for: " <> toText (show path)
        help = "Ensure valid Package.resolved exists, and is readable by user"
        documentationReferences = [swiftPackageResolvedRef, xcodeCoordinatePkgVersion, swiftFossaDocUrl]
    DiagnosticInfo Nothing Nothing (Just documentationReferences) Nothing (Just help) (Just ctx) Nothing
