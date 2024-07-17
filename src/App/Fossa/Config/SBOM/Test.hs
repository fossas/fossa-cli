{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.SBOM.Test (
  subcommand,
  parser,
  validateOutputFormat,
  testOutputFormatList,
  defaultOutputFmt,
  parseFossaTestOutputFormat,
  testFormatHelp,
  mergeOpts,
  SBOMTestOptions (..),
) where

import App.Fossa.Config.Common (
  CacheAction (..),
  CommonOpts (..),
  collectApiOpts,
  collectRevisionOverride,
  commonOpts,
  defaultTimeoutDuration,
 )
import App.Fossa.Config.ConfigFile (ConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.SBOM.Common (SBOMFile, getProjectRevision, sbomFileArg)
import App.Fossa.Config.Test (DiffRevision (DiffRevision), TestConfig (..), TestOutputFormat (..))
import App.Fossa.Subcommand (GetCommonOpts (getCommonOpts), GetSeverity (getSeverity))
import App.Types (BaseDir (BaseDir), LocatorType (..), OverrideProject (..))
import Control.Carrier.Diagnostics qualified as Diag
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  ToDiagnostic,
 )
import Control.Effect.Lift (Lift)
import Control.Timeout (Duration (..))
import Data.List (intercalate)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Effect.Logger (Severity (SevDebug, SevInfo), vsep)
import Effect.ReadFS (ReadFS, getCurrentDir)
import Errata (Errata (..))
import Options.Applicative (
  Parser,
  auto,
  helpDoc,
  long,
  option,
  optional,
  progDescDoc,
  strOption,
 )
import Options.Applicative.Builder (CommandFields, Mod, command, info)
import Prettyprinter (Doc, punctuate, viaShow)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Green))
import Style (applyFossaStyle, boldItalicized, coloredBoldItalicized, formatDoc, formatStringToDoc, stringToHelpDoc, styledDivider)

defaultOutputFmt :: TestOutputFormat
defaultOutputFmt = TestOutputPretty

testOutputFormatList :: String
testOutputFormatList = intercalate ", " $ map show allFormats

allFormats :: [TestOutputFormat]
allFormats = enumFromTo minBound maxBound

styledOutputFormats :: Doc AnsiStyle
styledOutputFormats = mconcat $ punctuate styledDivider coloredAllFormats
  where
    coloredAllFormats :: [Doc AnsiStyle]
    coloredAllFormats = map (coloredBoldItalicized Green . viaShow) allFormats

testFormatHelp :: Maybe (Doc AnsiStyle)
testFormatHelp =
  Just . formatDoc $
    vsep
      [ "Output the report in the specified format"
      , boldItalicized "Formats: " <> styledOutputFormats
      ]

newtype InvalidReportFormat = InvalidReportFormat String
instance ToDiagnostic InvalidReportFormat where
  renderDiagnostic (InvalidReportFormat fmt) = do
    let header = "Fossa test format: " <> toText fmt <> " is not supported"
        body = "Supported formats: " <> toText testOutputFormatList
    Errata (Just header) [] (Just body)

validateOutputFormat :: Has Diagnostics sig m => Maybe String -> m TestOutputFormat
validateOutputFormat Nothing = pure defaultOutputFmt
validateOutputFormat (Just format) = Diag.fromMaybe (InvalidReportFormat format) $ parseFossaTestOutputFormat format

parseFossaTestOutputFormat :: String -> Maybe TestOutputFormat
parseFossaTestOutputFormat "json" = Just TestOutputJson
parseFossaTestOutputFormat "text-pretty" = Just TestOutputPretty
parseFossaTestOutputFormat _ = Nothing

data SBOMTestOptions = SBOMTestOptions
  { testCommons :: CommonOpts
  , testTimeout :: Maybe Int
  , testOutputFmt :: Maybe String
  , testDiffRevision :: Maybe Text
  , sbomFile :: App.Fossa.Config.SBOM.Common.SBOMFile
  }
  deriving (Eq, Ord, Show)

instance GetSeverity SBOMTestOptions where
  getSeverity SBOMTestOptions{testCommons = CommonOpts{optDebug}} = if optDebug then SevDebug else SevInfo

subcommand :: (SBOMTestOptions -> a) -> Mod CommandFields a
subcommand f =
  command
    "test"
    ( info (f <$> parser) $
        progDescDoc (formatStringToDoc "Scan an SBOM file")
    )

instance GetCommonOpts SBOMTestOptions where
  getCommonOpts SBOMTestOptions{testCommons} = Just testCommons

parser :: Parser SBOMTestOptions
parser =
  SBOMTestOptions
    <$> commonOpts
    <*> optional (option auto (applyFossaStyle <> long "timeout" <> helpDoc timeoutHelp))
    <*> optional (strOption (applyFossaStyle <> long "format" <> helpDoc testFormatHelp))
    <*> optional (strOption (applyFossaStyle <> long "diff" <> stringToHelpDoc "Checks for new issues of the revision that does not exist in provided diff revision"))
    <*> App.Fossa.Config.SBOM.Common.sbomFileArg
  where
    timeoutHelp :: Maybe (Doc AnsiStyle)
    timeoutHelp =
      Just . formatDoc $
        vsep
          [ "Duration to wait for build completion in seconds"
          , boldItalicized "Default: " <> "3600 (1 hour)"
          ]

mergeOpts ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  SBOMTestOptions ->
  m TestConfig
mergeOpts maybeConfig envvars SBOMTestOptions{..} = do
  baseDir <- getCurrentDir
  let timeout = maybe defaultTimeoutDuration Seconds testTimeout
      diffRevision = DiffRevision <$> testDiffRevision

      revOverride =
        collectRevisionOverride maybeConfig $
          OverrideProject
            (optProjectName testCommons)
            (optProjectRevision testCommons)
            (Nothing)

  revision <- App.Fossa.Config.SBOM.Common.getProjectRevision sbomFile revOverride ReadOnly
  testOutputFormat <- validateOutputFormat testOutputFmt
  apiOpts <- collectApiOpts maybeConfig envvars testCommons

  pure $
    TestConfig
      { baseDir = (BaseDir baseDir)
      , apiOpts = apiOpts
      , timeout = timeout
      , outputFormat = testOutputFormat
      , projectRevision = revision
      , diffRevision = diffRevision
      , locatorType = LocatorTypeSBOM
      }
