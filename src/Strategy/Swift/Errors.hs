module Strategy.Swift.Errors (
  MissingPackageResolvedFile (..),
  MissingPackageResolvedFileHelp (..),
  SkippedComputedDependencies (..),

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

-- | Package.swift builds (some of) its dependencies with Swift code the CLI cannot evaluate,
-- e.g. `dependencies: dependencies` or `dependencies: generateDependencies()`.
newtype SkippedComputedDependencies = SkippedComputedDependencies (Path Abs File)

instance ToDiagnostic SkippedComputedDependencies where
  renderDiagnostic :: SkippedComputedDependencies -> Errata
  renderDiagnostic (SkippedComputedDependencies path) = do
    let header = "Some dependencies in " <> toText (show path) <> " are computed by Swift code, which FOSSA CLI cannot evaluate"
    let body = "Those dependencies are only reported from Package.resolved, as transitive dependencies. Declare them as `.package(...)` literals to have them reported as direct dependencies."
    Errata (Just header) [] (Just body)
