{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.SBOM.Test (
  mkSubCommand,
  subcommand,
  parser,
  loadConfig,
  validateOutputFormat,
  testOutputFormatList,
  defaultOutputFmt,
  parseFossaTestOutputFormat,
  testFormatHelp,
  mergeOpts,
  SBOMTestCliOpts (..),
) where

import App.Fossa.Config.Common (
  CacheAction (ReadOnly),
  CommonOpts (..),
  baseDirArg,
  collectApiOpts,
  collectBaseDir,
  collectRevisionData',
  commonOpts,
  defaultTimeoutDuration,
 )
import App.Fossa.Config.ConfigFile (ConfigFile, resolveConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.Test (DiffRevision (DiffRevision), TestConfig (TestConfig), TestOutputFormat (..))
import App.Fossa.Subcommand (EffStack, GetCommonOpts (getCommonOpts), GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Types (IssueLocatorType (..), OverrideProject (OverrideProject))
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  ToDiagnostic,
  fromMaybe,
 )
import Control.Effect.Lift (Lift)
import Control.Timeout (Duration (..))
import Data.List (intercalate)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Effect.Exec (Exec)
import Effect.Logger (Logger, Severity (SevDebug, SevInfo), vsep)
import Effect.ReadFS (ReadFS, getCurrentDir, resolveDir)
import Errata (Errata (..))
import Options.Applicative (
  InfoMod,
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
validateOutputFormat (Just format) = fromMaybe (InvalidReportFormat format) $ parseFossaTestOutputFormat format

parseFossaTestOutputFormat :: String -> Maybe TestOutputFormat
parseFossaTestOutputFormat "json" = Just TestOutputJson
parseFossaTestOutputFormat "text-pretty" = Just TestOutputPretty
parseFossaTestOutputFormat _ = Nothing

data SBOMTestCliOpts = SBOMTestCliOpts
  { testCommons :: CommonOpts
  , testTimeout :: Maybe Int
  , testOutputFmt :: Maybe String
  , testBaseDir :: FilePath
  , testDiffRevision :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance GetSeverity SBOMTestCliOpts where
  getSeverity SBOMTestCliOpts{testCommons = CommonOpts{optDebug}} = if optDebug then SevDebug else SevInfo

subcommand :: (SBOMTestCliOpts -> a) -> Mod CommandFields a
subcommand f =
  command
    "test"
    ( info (f <$> parser) $
        progDescDoc (formatStringToDoc "Scan an SBOM file")
    )

instance GetCommonOpts SBOMTestCliOpts where
  getCommonOpts SBOMTestCliOpts{testCommons} = Just testCommons

testInfo :: InfoMod a
testInfo = progDescDoc $ formatStringToDoc "Check for issues from FOSSA and exit non-zero when issues are found"

mkSubCommand :: (TestConfig -> EffStack ()) -> SubCommand SBOMTestCliOpts TestConfig
mkSubCommand = SubCommand "test" testInfo parser loadConfig mergeOpts

parser :: Parser SBOMTestCliOpts
parser =
  SBOMTestCliOpts
    <$> commonOpts
    <*> optional (option auto (applyFossaStyle <> long "timeout" <> helpDoc timeoutHelp))
    <*> optional (strOption (applyFossaStyle <> long "format" <> helpDoc testFormatHelp))
    <*> baseDirArg
    <*> optional (strOption (applyFossaStyle <> long "diff" <> stringToHelpDoc "Checks for new issues of the revision that does not exist in provided diff revision"))
  where
    timeoutHelp :: Maybe (Doc AnsiStyle)
    timeoutHelp =
      Just . formatDoc $
        vsep
          [ "Duration to wait for build completion in seconds"
          , boldItalicized "Default: " <> "3600 (1 hour)"
          ]

loadConfig ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  SBOMTestCliOpts ->
  m (Maybe ConfigFile)
loadConfig SBOMTestCliOpts{testCommons = CommonOpts{optConfig}, testBaseDir} = do
  cwd <- getCurrentDir
  configBaseDir <- resolveDir cwd (toText testBaseDir)
  resolveConfigFile configBaseDir optConfig

mergeOpts ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  SBOMTestCliOpts ->
  m TestConfig
mergeOpts maybeConfig envvars SBOMTestCliOpts{..} = do
  let baseDir = collectBaseDir testBaseDir
      apiOpts = collectApiOpts maybeConfig envvars testCommons
      timeout = maybe defaultTimeoutDuration Seconds testTimeout
      revision =
        collectRevisionData' baseDir maybeConfig ReadOnly $
          OverrideProject (optProjectName testCommons) (optProjectRevision testCommons) Nothing
      diffRevision = DiffRevision <$> testDiffRevision

  testOutputFormat <- validateOutputFormat testOutputFmt

  TestConfig
    <$> baseDir
    <*> apiOpts
    <*> pure timeout
    <*> pure testOutputFormat
    <*> revision
    <*> pure diffRevision
    <*> pure IssueLocatorSBOM
