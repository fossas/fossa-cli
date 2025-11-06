module App.Fossa.Config.Snippets (
  mkSubCommand,
  SnippetsConfig (..),
  SnippetsCommand,
  CommitOutputFormat (..),
  SnippetKind (..),
  SnippetTarget (..),
  SnippetTransform (..),
  AnalyzeConfig (..),
  CommitConfig (..),
  labelForKind,
  labelForTarget,
  labelForTransform,
) where

import App.Fossa.DebugDir (DebugDirRef)
import App.Fossa.Config.Common (apiKeyOpt, baseDirArg, collectBaseDir, endpointOpt)
import App.Fossa.Subcommand (EffStack, GetCommonOpts, GetSeverity (..), SubCommand (..))
import App.Types (BaseDir)
import Control.Carrier.Lift (sendIO)
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Lift (Lift)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.List qualified as List
import Data.String.Conversion (ToString, ToText, toString, toText)
import Data.Text (Text)
import Effect.Logger (Severity (..))
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Options.Applicative (InfoMod, Parser, command, eitherReader, info, long, many, metavar, option, optional, progDescDoc, short, strOption, subparser, switch, (<|>))
import Path (Abs, Dir, Path)
import Path.IO qualified as Path
import Style (applyFossaStyle, formatStringToDoc, stringToHelpDoc)
import Text.URI (URI)

data SnippetsCommand
  = CommandAnalyze
      FilePath -- Scan root
      Bool -- Debug
      (Maybe URI) -- The FOSSA endpoint. Not currently used, but accepted for backwards compatibility.
      (Maybe Text) -- The FOSSA API key. Not currently used, but accepted for backwards compatibility.
      FilePath -- Output directory
      Bool -- Whether to overwrite output directory
      [SnippetTarget]
      [SnippetKind]
      [SnippetTransform]
  | CommandCommit
      FilePath -- Scan root
      Bool -- Debug
      (Maybe URI) -- The FOSSA endpoint. Not currently used, but accepted for backwards compatibility.
      (Maybe Text) -- The FOSSA API key. Not currently used, but accepted for backwards compatibility.
      FilePath -- Analyze's output directory
      Bool -- Whether to overwrite output file
      (Maybe CommitOutputFormat)
      [SnippetTarget]
      [SnippetKind]
      [SnippetTransform]

snippetsInfo :: InfoMod a
snippetsInfo = progDescDoc $ formatStringToDoc "FOSSA snippet scanning"

snippetsAnalyzeInfo :: InfoMod a
snippetsAnalyzeInfo = progDescDoc $ formatStringToDoc "Analyze a local project for snippet matches"

snippetsCommitInfo :: InfoMod a
snippetsCommitInfo = progDescDoc $ formatStringToDoc "Commit matches discovered during analyze into a fossa-deps file"

instance GetSeverity SnippetsCommand where
  getSeverity :: SnippetsCommand -> Severity
  getSeverity (CommandAnalyze _ analyzeDebug _ _ _ _ _ _ _) = if analyzeDebug then SevDebug else SevInfo
  getSeverity (CommandCommit _ commitDebug _ _ _ _ _ _ _ _) = if commitDebug then SevDebug else SevInfo

instance GetCommonOpts SnippetsCommand

mkSubCommand :: (SnippetsConfig -> EffStack ()) -> SubCommand SnippetsCommand SnippetsConfig
mkSubCommand = SubCommand "snippets" snippetsInfo cliParser noLoadConfig mergeOpts
  where
    noLoadConfig = const $ pure Nothing

cliParser :: Parser SnippetsCommand
cliParser = analyze <|> commit
  where
    analyze = subparser . command "analyze" $ info analyzeOpts snippetsAnalyzeInfo
    analyzeOpts =
      CommandAnalyze
        <$> baseDirArg
        <*> switch (applyFossaStyle <> long "debug" <> stringToHelpDoc "Enable debug logging")
        <*> endpointOpt
        <*> apiKeyOpt
        <*> strOption (applyFossaStyle <> long "output" <> short 'o' <> stringToHelpDoc "The directory to which matches are output")
        <*> switch (applyFossaStyle <> long "overwrite-output" <> stringToHelpDoc "If specified, overwrites the output directory if it exists")
        <*> many (option (eitherReader parseTarget) (applyFossaStyle <> long "target" <> stringToHelpDoc "Analyze this combination of targets" <> metavar "TARGET"))
        <*> many (option (eitherReader parseKind) (applyFossaStyle <> long "kind" <> stringToHelpDoc "Analyze this combination of kinds" <> metavar "KIND"))
        <*> many (option (eitherReader parseTransform) (applyFossaStyle <> long "transform" <> stringToHelpDoc "Analyze this combination of transforms" <> metavar "TRANSFORM"))
    commit = subparser . command "commit" $ info commitOpts snippetsCommitInfo
    commitOpts =
      CommandCommit
        <$> baseDirArg
        <*> switch (applyFossaStyle <> long "debug" <> stringToHelpDoc "Enable debug logging")
        <*> endpointOpt
        <*> apiKeyOpt
        <*> strOption (applyFossaStyle <> long "analyze-output" <> stringToHelpDoc "The directory to which 'analyze' matches were saved")
        <*> switch (applyFossaStyle <> long "overwrite-fossa-deps" <> stringToHelpDoc "If specified, overwrites the 'fossa-deps' file if it exists")
        <*> optional (option (eitherReader parseCommitOutputFormat) (applyFossaStyle <> long "format" <> stringToHelpDoc "The output format for the generated `fossa-deps` file" <> metavar "FORMAT"))
        <*> many (option (eitherReader parseTarget) (applyFossaStyle <> long "target" <> stringToHelpDoc "Commit this combination of targets" <> metavar "TARGET"))
        <*> many (option (eitherReader parseKind) (applyFossaStyle <> long "kind" <> stringToHelpDoc "Commit this combination of kinds" <> metavar "KIND"))
        <*> many (option (eitherReader parseTransform) (applyFossaStyle <> long "transform" <> stringToHelpDoc "Commit this combination of transforms" <> metavar "TRANSFORM"))

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  DebugDirRef ->
  a ->
  b ->
  SnippetsCommand ->
  m SnippetsConfig
mergeOpts _ _ _ (CommandAnalyze path debug _ _ output overwrite targets kinds transforms) = do
  root <- collectBaseDir path
  output' <- sendIO $ Path.resolveDir' output
  pure . Analyze $ AnalyzeConfig root debug output' overwrite targets kinds transforms
mergeOpts _ _ _ (CommandCommit path debug _ _ analyzeOutput overwrite format targets kinds transforms) = do
  root <- collectBaseDir path
  analyzeOutput' <- sendIO $ Path.resolveDir' analyzeOutput
  pure . Commit $ CommitConfig root debug analyzeOutput' overwrite format targets kinds transforms

data SnippetsConfig
  = Analyze AnalyzeConfig
  | Commit CommitConfig
  deriving (Show, Generic)

instance ToJSON SnippetsConfig where
  toEncoding = genericToEncoding defaultOptions

data AnalyzeConfig = AnalyzeConfig
  { analyzeScanDir :: BaseDir
  , analyzeDebug :: Bool
  , analyzeOutput :: Path Abs Dir
  , analyzeOverwriteOutput :: Bool
  , analyzeTargets :: [SnippetTarget]
  , analyzeKinds :: [SnippetKind]
  , analyzeTransforms :: [SnippetTransform]
  }
  deriving (Show, Generic)

instance ToJSON AnalyzeConfig where
  toEncoding = genericToEncoding defaultOptions

data CommitConfig = CommitConfig
  { commitScanDir :: BaseDir
  , commitDebug :: Bool
  , commitAnalyzeOutput :: Path Abs Dir
  , commitOverwriteFossaDeps :: Bool
  , commitOutputFormat :: Maybe CommitOutputFormat
  , commitTargets :: [SnippetTarget]
  , commitKinds :: [SnippetKind]
  , commitTransforms :: [SnippetTransform]
  }
  deriving (Show, Generic)

instance ToJSON CommitConfig where
  toEncoding = genericToEncoding defaultOptions

-- | The targets of snippets to extract.
-- Reference: @millhone::extract::Target@.
data SnippetTarget
  = SnippetTargetFunction
  deriving (Eq, Enum, Bounded, Show, Generic)

instance ToJSON SnippetTarget where
  toEncoding = genericToEncoding defaultOptions

parseTarget :: String -> Either String SnippetTarget
parseTarget input = case List.find (\t -> toString t == input) optionsTarget of
  Just found -> Right found
  Nothing -> Left $ generateParseError input (toString <$> optionsTarget)

optionsTarget :: [SnippetTarget]
optionsTarget = enumFromTo minBound maxBound

instance ToText SnippetTarget where
  toText :: SnippetTarget -> Text
  toText SnippetTargetFunction = "function"

instance ToString SnippetTarget where
  toString :: SnippetTarget -> String
  toString = toString . toText

labelForTarget :: Text
labelForTarget = "--target"

-- | The kind of item this snippet represents.
-- Reference: @millhone::extract::Kind@.
data SnippetKind
  = SnippetKindSignature
  | SnippetKindBody
  | SnippetKindFull
  deriving (Eq, Enum, Bounded, Show, Generic)

instance ToJSON SnippetKind where
  toEncoding = genericToEncoding defaultOptions

instance ToText SnippetKind where
  toText :: SnippetKind -> Text
  toText SnippetKindSignature = "signature"
  toText SnippetKindBody = "body"
  toText SnippetKindFull = "full"

instance ToString SnippetKind where
  toString :: SnippetKind -> String
  toString = toString . toText

parseKind :: String -> Either String SnippetKind
parseKind input = case List.find (\t -> toString t == input) optionsKind of
  Just found -> Right found
  Nothing -> Left $ generateParseError input (toString <$> optionsKind)

optionsKind :: [SnippetKind]
optionsKind = enumFromTo minBound maxBound

labelForKind :: Text
labelForKind = "--kind"

-- | The normalization used to extract this snippet.
-- Reference: @millhone::extract::Transform@.
data SnippetTransform
  = SnippetTransformCode
  | SnippetTransformComment
  | SnippetTransformSpace
  deriving (Eq, Enum, Bounded, Show, Generic)

instance ToJSON SnippetTransform where
  toEncoding = genericToEncoding defaultOptions

instance ToText SnippetTransform where
  toText :: SnippetTransform -> Text
  toText SnippetTransformCode = "code"
  toText SnippetTransformComment = "comment"
  toText SnippetTransformSpace = "space"

instance ToString SnippetTransform where
  toString :: SnippetTransform -> String
  toString = toString . toText

parseTransform :: String -> Either String SnippetTransform
parseTransform input = case List.find (\t -> toString t == input) optionsTransform of
  Just found -> Right found
  Nothing -> Left $ generateParseError input (toString <$> optionsTransform)

optionsTransform :: [SnippetTransform]
optionsTransform = enumFromTo minBound maxBound

labelForTransform :: Text
labelForTransform = "--transform"

data CommitOutputFormat
  = Yml
  | Json
  deriving (Eq, Enum, Bounded, Show, Generic)

instance ToJSON CommitOutputFormat where
  toEncoding = genericToEncoding defaultOptions

instance ToText CommitOutputFormat where
  toText :: CommitOutputFormat -> Text
  toText Yml = "yml"
  toText Json = "json"

instance ToString CommitOutputFormat where
  toString :: CommitOutputFormat -> String
  toString = toString . toText

parseCommitOutputFormat :: String -> Either String CommitOutputFormat
parseCommitOutputFormat input = case List.find (\t -> toString t == input) optionsCommitOutputFormat of
  Just found -> Right found
  Nothing -> Left $ generateParseError input (toString <$> optionsCommitOutputFormat)

optionsCommitOutputFormat :: [CommitOutputFormat]
optionsCommitOutputFormat = enumFromTo minBound maxBound

generateParseError :: String -> [String] -> String
generateParseError input options = "'" <> input <> "' is not a valid option; expected one of: " <> List.intercalate ", " options
