module App.Fossa.LicenseScan (
  licenseScanSubCommand,
) where

import App.Fossa.Config.ConfigFile (ConfigFile (configVendoredDependencies), VendoredDependencyConfigs (configLicenseScanPathFilters), resolveConfigFile)
import App.Fossa.Config.LicenseScan (
  LicenseScanCommand,
  LicenseScanConfig (..),
  mkSubCommand,
 )
import App.Fossa.EmbeddedBinary (withThemisAndIndex)
import App.Fossa.LicenseScanner (scanVendoredDep)
import App.Fossa.ManualDeps (
  ManualDependencies (vendoredDependencies),
  findFossaDepsFile,
  readFoundDeps,
 )
import App.Fossa.RunThemis (execRawThemis)
import App.Fossa.Subcommand (SubCommand)
import App.Fossa.VendoredDependency (
  VendoredDependency,
  dedupVendoredDeps,
 )
import App.Types (BaseDir (BaseDir), FullFileUploads (FullFileUploads))
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.StickyLogger (
  Has,
  StickyLogger,
  runStickyLogger,
 )
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, fromMaybe)
import Control.Effect.Lift (Lift)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Data.Aeson qualified as Aeson
import Data.Error (SourceLocation, createBlock, getSourceLocation)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.String.Conversion (decodeUtf8)
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Effect.Exec (Exec)
import Effect.Logger (Logger, Severity (SevInfo), logStdout, renderIt)
import Effect.ReadFS (ReadFS)
import Errata (errataSimple)
import Path (Abs, Dir, Path)
import Prettyprinter (vsep)
import Srclib.Types (LicenseSourceUnit)
import Types (LicenseScanPathFilters)

newtype MissingFossaDepsFile = MissingFossaDepsFile SourceLocation
newtype NoVendoredDeps = NoVendoredDeps SourceLocation

instance ToDiagnostic MissingFossaDepsFile where
  renderDiagnostic (MissingFossaDepsFile srcLoc) = do
    let header = "Missing fossa-deps file"
        body =
          renderIt $
            vsep
              [ "'fossa license-scan fossa-deps' requires pointing to a directory with a fossa-deps file."
              , "The file can have one of the extensions: .yaml .yml .json"
              ]
        block = createBlock srcLoc Nothing Nothing
    errataSimple (Just header) block (Just body)

instance ToDiagnostic NoVendoredDeps where
  renderDiagnostic (NoVendoredDeps srcLoc) = do
    let header = "The 'vendored-dependencies' section of the fossa deps file is empty or missing."
        block = createBlock srcLoc Nothing Nothing
    errataSimple (Just header) block Nothing

newtype UploadUnits = UploadUnits (NonEmpty LicenseSourceUnit)

instance ToJSON UploadUnits where
  toJSON (UploadUnits units) = object ["uploadUnits" .= units]

licenseScanSubCommand :: SubCommand LicenseScanCommand LicenseScanConfig
licenseScanSubCommand = mkSubCommand licenseScanMain

licenseScanMain ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  LicenseScanConfig ->
  m ()
licenseScanMain = \case
  RawPathScan (BaseDir dir) -> logStdout . decodeUtf8 =<< withThemisAndIndex (\bins -> execRawThemis bins dir ["--srclib-with-matches"])
  VendoredDepsOutput basedir -> outputVendoredDeps basedir

outputVendoredDeps ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  ) =>
  BaseDir ->
  m ()
outputVendoredDeps (BaseDir dir) = runStickyLogger SevInfo $ do
  config <- resolveConfigFile dir Nothing
  manualDepsFile <- fromMaybe (MissingFossaDepsFile getSourceLocation) =<< findFossaDepsFile dir
  manualDeps <- readFoundDeps manualDepsFile
  vendoredDeps <- fromMaybe (NoVendoredDeps getSourceLocation) $ NE.nonEmpty $ vendoredDependencies manualDeps
  let licenseScanPathFilters = config >>= configVendoredDependencies >>= configLicenseScanPathFilters
  resultMap <- UploadUnits <$> runLicenseScan dir licenseScanPathFilters vendoredDeps
  logStdout . decodeUtf8 $ Aeson.encode resultMap

-- runLicenseScan does not require an API key, so we can't get the FullFileUploads param from the organization,
-- so we just default FullFileUploads to False.
runLicenseScan ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  , Has StickyLogger sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  ) =>
  Path Abs Dir ->
  Maybe LicenseScanPathFilters ->
  NonEmpty VendoredDependency ->
  m (NonEmpty LicenseSourceUnit)
runLicenseScan basedir licenseScanPathFilters vdeps = dedupVendoredDeps vdeps >>= traverse (scanVendoredDep basedir licenseScanPathFilters $ FullFileUploads False)
