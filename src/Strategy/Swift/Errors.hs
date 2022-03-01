module Strategy.Swift.Errors (
  MissingPackageResolvedFile (..),

  -- * docs
  swiftFossaDocUrl,
  swiftPackageResolvedRef,
  xcodeCoordinatePkgVersion,
) where

import App.Docs (platformDocUrl)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
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
  renderDiagnostic (MissingPackageResolvedFile path) =
    vsep
      [ "We could not perform Package.resolved analysis for: " <> viaShow path
      , ""
      , indent 2 $
          vsep
            [ "Ensure valid Package.resolved exists, and is readable by user."
            ]
      , ""
      , "Refer to:"
      , indent 2 $ pretty $ "- " <> swiftPackageResolvedRef
      , indent 2 $ pretty $ "- " <> xcodeCoordinatePkgVersion
      , indent 2 $ pretty $ "- " <> swiftFossaDocUrl
      ]
