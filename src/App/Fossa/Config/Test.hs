{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Test (
  TestCliOpts,
  TestConfig (..),
  DiffRevision (..),
  OutputFormat (..),
  mkSubCommand,
  parser,
  loadConfig,
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
import App.Fossa.Subcommand (EffStack, GetCommonOpts (getCommonOpts), GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Types (BaseDir, OverrideProject (OverrideProject), ProjectRevision)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
 )
import Control.Effect.Lift (Lift)
import Control.Timeout (Duration (..))
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Effect.Exec (Exec)
import Effect.Logger (Logger, Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS, getCurrentDir, resolveDir)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)
import Options.Applicative (
  InfoMod,
  Parser,
  auto,
  flag,
  help,
  long,
  option,
  optional,
  progDesc,
  strOption,
 )

data OutputFormat
  = TestOutputPretty
  | TestOutputJson
  deriving (Eq, Ord, Show, Generic)

instance ToJSON OutputFormat where
  toEncoding = genericToEncoding defaultOptions

newtype DiffRevision = DiffRevision Text deriving (Show, Eq, Ord, Generic)

instance ToJSON DiffRevision where
  toEncoding = genericToEncoding defaultOptions

data TestCliOpts = TestCliOpts
  { commons :: CommonOpts
  , testTimeout :: Maybe Int
  , testOutputType :: OutputFormat
  , testBaseDir :: FilePath
  , testDiffRevision :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance GetSeverity TestCliOpts where
  getSeverity TestCliOpts{commons = CommonOpts{optDebug}} = if optDebug then SevDebug else SevInfo

instance GetCommonOpts TestCliOpts where
  getCommonOpts TestCliOpts{commons} = Just commons

data TestConfig = TestConfig
  { baseDir :: BaseDir
  , apiOpts :: ApiOpts
  , timeout :: Duration
  , outputFormat :: OutputFormat
  , projectRevision :: ProjectRevision
  , diffRevision :: Maybe DiffRevision
  }
  deriving (Show, Generic)

instance ToJSON TestConfig where
  toEncoding = genericToEncoding defaultOptions

testInfo :: InfoMod a
testInfo = progDesc "Check for issues from FOSSA and exit non-zero when issues are found"

mkSubCommand :: (TestConfig -> EffStack ()) -> SubCommand TestCliOpts TestConfig
mkSubCommand = SubCommand "test" testInfo parser loadConfig mergeOpts

parser :: Parser TestCliOpts
parser =
  TestCliOpts
    <$> commonOpts
    <*> optional (option auto (long "timeout" <> help "Duration to wait for build completion in seconds (Defaults to 1 hour)"))
    <*> flag TestOutputPretty TestOutputJson (long "json" <> help "Output issues as json")
    <*> baseDirArg
    <*> optional (strOption (long "diff" <> help "Checks for new issues of the revision, that does not exist in provided diff revision."))

loadConfig ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  TestCliOpts ->
  m (Maybe ConfigFile)
loadConfig TestCliOpts{commons = CommonOpts{optConfig}, testBaseDir} = do
  cwd <- getCurrentDir
  configBaseDir <- resolveDir cwd (toText testBaseDir)
  resolveConfigFile configBaseDir optConfig

mergeOpts ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  TestCliOpts ->
  m TestConfig
mergeOpts maybeConfig envvars TestCliOpts{..} = do
  let baseDir = collectBaseDir testBaseDir
      apiOpts = collectApiOpts maybeConfig envvars commons
      timeout = maybe defaultTimeoutDuration Seconds testTimeout
      revision =
        collectRevisionData' baseDir maybeConfig ReadOnly $
          OverrideProject (optProjectName commons) (optProjectRevision commons) Nothing
      diffRevision = DiffRevision <$> testDiffRevision

  TestConfig
    <$> baseDir
    <*> apiOpts
    <*> pure timeout
    <*> pure testOutputType
    <*> revision
    <*> pure diffRevision
