{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Effect.Exec (
  argFromPath,
  argsLabeled,
  argsLabeledWith,
  Exec,
  ExecF (..),
  ExecErr (..),
  exec,
  execInCwd,
  execEffectful,
  execThrow,
  Command (..),
  CmdFailure (..),
  AllowErr (..),
  execParser,
  execJson,
  execJson',
  ExecIOC,
  runExecIO,
  renderCommand,
  module System.Exit,
  execThrow',
  execCurrentDirStdinThrow,
  Has,
  CandidateCommandEffs,
  CandidateAnalysisCommands (..),
  mkAnalysisCommand,
  mkSingleCandidateAnalysisCommand,
) where

import App.Support (reportDefectMsg)
import App.Types (OverrideDynamicAnalysisBinary (..))
import Control.Algebra (Has)
import Control.Carrier.Reader (Reader, ask)
import Control.Carrier.Simple (
  Simple,
  SimpleC,
  interpret,
  sendSimple,
 )
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic (..),
  context,
  fatal,
  fatalText,
  recover,
  warnOnErr,
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Record (RecordableValue (..))
import Control.Effect.Record.TH (deriveRecordable)
import Control.Effect.Replay (ReplayableValue (..))
import Control.Effect.Replay.TH (deriveReplayable)
import Control.Exception (IOException, try)
import Data.Aeson (
  FromJSON (parseJSON),
  KeyValue ((.=)),
  ToJSON (toJSON),
  eitherDecode,
  object,
  withObject,
  (.:),
 )
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Error (createBody)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.String (fromString)
import Data.String.Conversion (ToText, decodeUtf8, toStrict, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import DepTypes (DepType (..))
import Effect.Logger (Logger, logInfo, renderIt)
import Effect.ReadFS (ReadFS, getCurrentDir)
import Errata (Errata (..))
import GHC.Generics (Generic)
import Path (Abs, Dir, Path, SomeBase (..), fromAbsDir, toFilePath)
import Path.IO (AnyPath (makeAbsolute))
import Prettyprinter (Doc, indent, pretty, viaShow, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Exit (ExitCode (..))
import System.Process.Typed (
  proc,
  readProcess,
  setStdin,
  setWorkingDir,
 )
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Error (errorBundlePretty)

data Command = Command
  { cmdName :: Text
  -- ^ Command name to use. E.g., "pip", "pip3", "./gradlew".
  , cmdArgs :: [Text]
  -- ^ Arguments for the command
  , cmdAllowErr :: AllowErr
  -- ^ Error (i.e. non-zero exit code) tolerance policy for running commands. This is helpful for commands like @npm@, that nonsensically return non-zero exit codes when a command succeeds
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Command
instance RecordableValue Command
instance FromJSON Command
instance ReplayableValue Command

renderCommand :: Command -> Text
renderCommand (Command name args _) = Text.intercalate " " $ [name] <> args

data CmdFailure = CmdFailure
  { cmdFailureCmd :: Command
  , cmdFailureDir :: FilePath
  , cmdFailureExit :: ExitCode
  , cmdFailureStdout :: Stdout
  , cmdFailureStderr :: Stderr
  }
  deriving (Eq, Ord, Show)

instance ToJSON CmdFailure where
  toJSON CmdFailure{..} =
    object
      [ "cmdFailureCmd" .= cmdFailureCmd
      , "cmdFailureDir" .= cmdFailureDir
      , "cmdFailureExit" .= toRecordedValue cmdFailureExit
      , "cmdFailureStdout" .= toRecordedValue cmdFailureStdout
      , "cmdFailureStderr" .= toRecordedValue cmdFailureStderr
      ]
instance RecordableValue CmdFailure

instance FromJSON CmdFailure where
  parseJSON = withObject "CmdFailure" $ \obj ->
    CmdFailure
      <$> obj .: "cmdFailureCmd"
      <*> obj .: "cmdFailureDir"
      <*> (obj .: "cmdFailureExit" >>= fromRecordedValue)
      <*> (obj .: "cmdFailureStdout" >>= fromRecordedValue)
      <*> (obj .: "cmdFailureStderr" >>= fromRecordedValue)
instance ReplayableValue CmdFailure

data AllowErr
  = -- | never ignore non-zero exit (return 'ExecErr')
    Never
  | -- | when `stdout` is non-empty, ignore non-zero exit
    NonEmptyStdout
  | -- | always ignore non-zero exit
    Always
  deriving (Eq, Ord, Show, Generic)

instance ToJSON AllowErr
instance RecordableValue AllowErr
instance FromJSON AllowErr
instance ReplayableValue AllowErr

type Stdout = BL.ByteString

type Stderr = BL.ByteString

data ExecF a where
  -- | Exec runs a command and returns either:
  -- - stdout when the command succeeds
  -- - a description of the command failure
  Exec :: SomeBase Dir -> Command -> Maybe Text -> ExecF (Either CmdFailure Stdout)

type Exec = Simple ExecF

$(deriveRecordable ''ExecF)
$(deriveReplayable ''ExecF)

deriving instance Show (ExecF a)
deriving instance Eq (ExecF a)
deriving instance Ord (ExecF a)

data ExecErr
  = -- | Command execution failed, usually from a non-zero exit
    CommandFailed CmdFailure
  | -- | Command output couldn't be parsed. command, err
    CommandParseError Command Text
  | ExecEnvNotSupported Text
  deriving (Eq, Ord, Show, Generic)

renderCmdFailure :: CmdFailure -> Errata
renderCmdFailure CmdFailure{..} =
  if isCmdNotAvailable
    then do
      let header = "Could not find executable: `" <> cmdName cmdFailureCmd <> "`"
          help = "Please ensure `" <> cmdName cmdFailureCmd <> "` exists in PATH prior to running fossa"
          body = createBody Nothing Nothing (Just $ renderIt reportDefectMsg) (Just help) Nothing
      Errata (Just header) [] (Just body)
    else do
      let header = "Command execution failed"
          content = renderIt $ vsep [indent 2 details]
          body = createBody (Just content) Nothing (Just $ renderIt reportDefectMsg) Nothing Nothing
      Errata (Just header) [] (Just body)
  where
    -- Infer if the stderr is caused by not having executable in path.
    -- There is no easy way to check for @EBADF@ within process exception,
    -- with the library we use and effort required.
    isCmdNotAvailable :: Bool
    isCmdNotAvailable = expectedCmdNotFoundErrStr == stdErr

    expectedCmdNotFoundErrStr :: Text
    expectedCmdNotFoundErrStr = cmdName cmdFailureCmd <> ": startProcess: exec: invalid argument (Bad file descriptor)"

    prettyCommand :: Command -> Doc AnsiStyle
    prettyCommand Command{..} = pretty $ cmdName <> " " <> Text.intercalate " " cmdArgs

    stdErr :: Text
    stdErr = decodeUtf8 cmdFailureStderr

    stdOut :: Text
    stdOut = decodeUtf8 cmdFailureStdout

    exitCode :: Doc AnsiStyle
    exitCode = case cmdFailureExit of
      ExitSuccess -> "0"
      ExitFailure n -> viaShow n

    -- Render details of a failed command to error text.
    details :: Doc AnsiStyle
    details =
      vsep
        [ "Attempted to run the command '" <> prettyCommand cmdFailureCmd <> "'"
        , "inside the working directory '" <> pretty cmdFailureDir <> "',"
        , "but failed, because the command exited with code '" <> exitCode <> "'."
        , ""
        , "Often, this kind of error is caused by the project not being ready to build;"
        , "please ensure that the project at '" <> pretty cmdFailureDir <> "'"
        , "builds successfully before running fossa."
        , ""
        , outputPreamble
        , prettyStdOut
        , prettyStdErr
        ]

    outputPreamble :: Doc AnsiStyle
    outputPreamble =
      if Text.null stdOut && Text.null stdErr
        then
          vsep
            [ "The command did not log any output!"
            , "Please check the project documentation for this command for troubleshooting guidance."
            ]
        else
          vsep
            [ "The logs for the command are listed below."
            , "They will likely provide guidance on how to resolve this error."
            ]

    prettyStdOut :: Doc AnsiStyle
    prettyStdOut =
      if Text.null stdOut
        then "Command did not write a standard log."
        else
          vsep
            [ ""
            , "Command standard log:"
            , indent 2 $ pretty stdOut
            ]

    prettyStdErr :: Doc AnsiStyle
    prettyStdErr =
      if Text.null stdErr
        then "Command did not write an error log."
        else
          vsep
            [ ""
            , "Command error log:"
            , indent 2 $ pretty stdErr
            ]

instance ToDiagnostic ExecErr where
  renderDiagnostic :: ExecErr -> Errata
  renderDiagnostic = \case
    ExecEnvNotSupported env -> do
      let header = "Exec is not supported in: " <> env
      Errata (Just header) [] Nothing
    CommandFailed err -> renderCmdFailure err
    CommandParseError cmd err -> do
      let header = "Failed to parse command output"
          content = renderIt $ vsep [indent 2 (pretty err)]
          ctx = "Command: " <> toText (show cmd)
          body = createBody (Just content) Nothing (Just $ renderIt reportDefectMsg) Nothing (Just ctx)
      Errata (Just header) [] (Just body)

-- | Execute a command and return its @(exitcode, stdout, stderr)@
exec :: Has Exec sig m => Path Abs Dir -> Command -> m (Either CmdFailure Stdout)
exec dir cmd = sendSimple (Exec (Abs dir) cmd Nothing)

-- | A variant of 'exec' that runs the command in the current directory
execInCwd :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => Command -> m (Either CmdFailure Stdout)
execInCwd cmd = context ("Running command '" <> cmdName cmd <> "'") $ do
  dir <- getCurrentDir
  exec dir cmd

-- | Execute a command with stdin and return its @(exitcode, stdout, stderr)@
exec' :: Has Exec sig m => Path Abs Dir -> Command -> Text -> m (Either CmdFailure Stdout)
exec' dir cmd stdin = sendSimple (Exec (Abs dir) cmd (Just stdin))

type Parser = Parsec Void Text

-- | Parse the stdout of a command
execParser :: forall a sig m. (Has Exec sig m, Has Diagnostics sig m) => Parser a -> Path Abs Dir -> Command -> m a
execParser parser dir cmd = do
  stdout <- execThrow dir cmd
  case runParser parser "" (decodeUtf8 stdout) of
    Left err -> fatal (CommandParseError cmd (toText (errorBundlePretty err)))
    Right a -> pure a

-- | Parse the JSON stdout of a command
execJson :: (FromJSON a, Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> Command -> m a
execJson dir cmd = do
  stdout <- execThrow dir cmd
  case eitherDecode stdout of
    Left err -> fatal (CommandParseError cmd (toText (show err)))
    Right a -> pure a

-- | Parse the JSON stdout of a command, but executes command with stdin.
execJson' :: (FromJSON a, Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> Command -> Text -> m a
execJson' dir cmd stdin = do
  result <- exec' dir cmd stdin
  case result of
    Left failure -> fatal (CommandFailed failure)
    Right stdout ->
      case eitherDecode stdout of
        Left err -> fatal (CommandParseError cmd (toText (show err)))
        Right a -> pure a

-- | A variant of 'exec' that throws a 'ExecErr' when the command returns a non-zero exit code
execThrow :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> Command -> m BL.ByteString
execThrow dir cmd = context ("Running command '" <> cmdName cmd <> "'") $ do
  result <- exec dir cmd
  case result of
    Left failure -> fatal (CommandFailed failure)
    Right stdout -> pure stdout

-- | A variant of 'exec' that is run for its side effects:
-- * Throws an 'ExecErr' when the command returns a non-zero exit code, like @execThrow@.
-- * Logs each line of the subcommand's stdout via @logInfo@.
--
-- Note: currently this buffers subcommand output; a future version may stream instead.
execEffectful ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  Command ->
  m ()
execEffectful dir cmd = context ("Running command '" <> cmdName cmd <> "'") $ do
  result <- exec dir cmd
  case result of
    Left failure -> fatal (CommandFailed failure)
    Right stdout -> do
      let outputLines :: [Text] = decodeUtf8 . toStrict <$> BL.splitWith (== 10) stdout
      traverse_ (logInfo . pretty) outputLines

-- | A variant of 'execThrow' that runs the command in the current directory
execThrow' :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => Command -> m BL.ByteString
execThrow' cmd = context ("Running command '" <> cmdName cmd <> "'") $ do
  dir <- getCurrentDir
  execThrow dir cmd

-- | A variant of 'execThrow' that runs the command in the current directory and accepts stdin
execCurrentDirStdinThrow :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => Command -> Text -> m BL.ByteString
execCurrentDirStdinThrow cmd stdin = do
  dir <- getCurrentDir
  result <- exec' dir cmd stdin
  case result of
    Left failure -> fatal (CommandFailed failure)
    Right stdout -> pure stdout

-- | Shorthand for the effects needed to select a candidate analysis command.
type CandidateCommandEffs sig m = (Has Diagnostics sig m, Has Exec sig m, Has (Reader OverrideDynamicAnalysisBinary) sig m)

-- | Describe a set of command names and the arguments used to validate the command names.
-- Optionally, also specify the override kind for the command, which is used
-- to look for a potential override command provided by the user from the environment.
data CandidateAnalysisCommands = CandidateAnalysisCommands
  { candidateCmdNames :: NonEmpty Text
  , candidateCmdArgs :: [Text]
  , candidateOverrideKind :: Maybe DepType
  }
  deriving (Show)

-- | Convenience function for creating a @CandidateAnalysisCommands@ with a single candidate command.
mkSingleCandidateAnalysisCommand :: Text -> [Text] -> Maybe DepType -> CandidateAnalysisCommands
mkSingleCandidateAnalysisCommand cmd = CandidateAnalysisCommands (NE.singleton cmd)

-- | Create a @Command@ for dynamic analysis of a project of the given @DepType@ from the list of provided commands.
--
-- This function selects the appropriate binary to use given the environment
-- and creates the command with the provided args and error handling semantics.
--
-- It is also possible that no supported command is valid in the provided context;
-- in such a case a diagnostics error is thrown in @m@.
mkAnalysisCommand ::
  ( CandidateCommandEffs sig m
  ) =>
  CandidateAnalysisCommands ->
  Path Abs Dir ->
  [Text] ->
  AllowErr ->
  m Command
mkAnalysisCommand candidates@CandidateAnalysisCommands{..} workdir args allowErr =
  context ("Make analysis command from " <> toText (show candidates)) $ do
    (overrideBinaries :: OverrideDynamicAnalysisBinary) <- ask
    cmd <- case candidateOverrideKind of
      Just dt -> case Map.lookup dt (unOverrideDynamicAnalysisBinary overrideBinaries) of
        Nothing -> context "Command override supported, but not specified" $ selectBestCmd workdir candidates
        Just cmd -> context ("Command override provided: " <> cmd) . selectBestCmd workdir $ withCmdOverride cmd
      Nothing -> context "Override not supported for this command" $ selectBestCmd workdir candidates
    pure $ Command{cmdName = cmd, cmdArgs = args, cmdAllowErr = allowErr}
  where
    withCmdOverride :: Text -> CandidateAnalysisCommands
    withCmdOverride override =
      CandidateAnalysisCommands
        { candidateCmdNames = NE.cons override candidateCmdNames
        , candidateCmdArgs = candidateCmdArgs
        , candidateOverrideKind = candidateOverrideKind
        }

-- | Given a set of possible binaries to try, choose the best one to use for dynamic analysis of this @DepType@.
selectBestCmd :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> CandidateAnalysisCommands -> m Text
selectBestCmd workdir CandidateAnalysisCommands{..} = selectBestCmd' (NE.toList candidateCmdNames)
  where
    selectBestCmd' :: (Has Diagnostics sig m, Has Exec sig m) => [Text] -> m Text
    selectBestCmd' (cmd : remaining) = context ("Evaluate command: " <> cmd) $ do
      let attempt = Command{cmdName = cmd, cmdArgs = candidateCmdArgs, cmdAllowErr = Never}
      output <- recover . warnOnErr (CandidateCommandFailed cmd candidateCmdArgs) $ execThrow workdir attempt
      case output of
        Nothing -> selectBestCmd' remaining
        Just _ -> pure cmd
    selectBestCmd' [] = fatalText "unable to select best binary to analyze project: none passed validation"

data CandidateCommandFailed = CandidateCommandFailed {failedCommand :: Text, failedArgs :: [Text]}
instance ToDiagnostic CandidateCommandFailed where
  renderDiagnostic :: CandidateCommandFailed -> Errata
  renderDiagnostic CandidateCommandFailed{..} = do
    let header = "Command: " <> "`" <> failedCommand <> "` not suitable"
        body = "Running with args: " <> "`" <> mconcat failedArgs <> "` resulted in a non-zero exit code"
    Errata (Just header) [] (Just body)

argFromPath :: Path a b -> Text
argFromPath = toText . toFilePath

argsLabeled :: ToText a => Text -> [a] -> [Text]
argsLabeled = argsLabeledWith toText

argsLabeledWith :: (a -> Text) -> Text -> [a] -> [Text]
argsLabeledWith render label (arg : args) = [label, render arg] ++ argsLabeledWith render label args
argsLabeledWith _ _ [] = []

type ExecIOC = SimpleC ExecF

runExecIO :: Has (Lift IO) sig m => ExecIOC m a -> m a
runExecIO = interpret $ \case
  Exec dir cmd stdin -> sendIO $ do
    absolute <-
      case dir of
        Abs absDir -> pure absDir
        Rel relDir -> makeAbsolute relDir

    let cmdName' = toString $ cmdName cmd
        cmdArgs' = map toString $ cmdArgs cmd

        mkFailure :: ExitCode -> Stdout -> Stderr -> CmdFailure
        mkFailure = CmdFailure cmd (fromAbsDir absolute)

        ioExceptionToCmdFailure :: IOException -> CmdFailure
        ioExceptionToCmdFailure = mkFailure (ExitFailure 1) "" . fromString . show

    let process = setWorkingDir (fromAbsDir absolute) (proc cmdName' cmdArgs')
    processResult <- try . readProcess $ case stdin of
      Just stdin' -> setStdin (fromString . toString $ stdin') process
      Nothing -> process

    -- apply business logic for considering whether exitcode + stderr constitutes a "failure"
    let mangleResult :: (ExitCode, Stdout, Stderr) -> Either CmdFailure Stdout
        mangleResult (exitcode, stdout, stderr) =
          case (exitcode, cmdAllowErr cmd) of
            (ExitSuccess, _) -> Right stdout
            (_, Never) -> Left $ mkFailure exitcode stdout stderr
            (_, NonEmptyStdout) ->
              if BL.null stdout
                then Left $ mkFailure exitcode stdout stderr
                else Right stdout
            (_, Always) -> Right stdout

    let result :: Either CmdFailure Stdout
        result = first ioExceptionToCmdFailure processResult >>= mangleResult

    pure result
