module Strategy.Swift.Errors (
  MissingPackageResolvedFile (..),
  MissingPackageResolvedFileHelp (..),

  -- * docs
  swiftFossaDocUrl,
  swiftPackageResolvedRef,
  xcodeCoordinatePkgVersion,
) where

import App.Docs (platformDocUrl)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
import Errata (Errata (..))
import Path

swiftFossaDocUrl :: Text
swiftFossaDocUrl = platformDocUrl "ios/swift.md"

swiftPackageResolvedRef :: Text
swiftPackageResolvedRef = "https://github.com/apple/swift-package-manager/blob/main/Documentation/Usage.md#resolving-versions-packageresolved-file"

xcodeCoordinatePkgVersion :: Text
xcodeCoordinatePkgVersion = "https://developer.apple.com/documentation/swift_packages/adding_package_dependencies_to_your_app"

newtype MissingPackageResolvedFile = MissingPackageResolvedFile (Path Abs File)
data MissingPackageResolvedFileHelp = MissingPackageResolvedFileHelp

instance ToDiagnostic MissingPackageResolvedFile where
  renderDiagnostic :: MissingPackageResolvedFile -> Errata
  renderDiagnostic (MissingPackageResolvedFile path) = do
    let header = "We could not perform Package.resolved analysis for: " <> toText (show path)
    Errata (Just header) [] Nothing

instance ToDiagnostic MissingPackageResolvedFileHelp where
  renderDiagnostic MissingPackageResolvedFileHelp = do
    let header = "Ensure valid Package.resolved exists, and is readable by user"
    Errata (Just header) [] Nothing
