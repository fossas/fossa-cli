{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Effect.Exec (
  Exec,
  ExecF (..),
  ExecErr (..),
  exec,
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
  Has,
) where

import App.Support (reportDefectMsg)
import Control.Algebra (Has)
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
import Data.String (fromString)
import Data.String.Conversion (decodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Effect.ReadFS (ReadFS, getCurrentDir)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path, SomeBase (..), fromAbsDir)
import Path.IO (AnyPath (makeAbsolute))
import Prettyprinter (Doc, indent, line, pretty, viaShow, vsep)
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

-- TODO: add a "shell command" method; this would help in App.Fossa.VPS.NinjaGraph
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
  deriving (Eq, Ord, Show, Generic)

renderCmdFailure :: CmdFailure -> Doc AnsiStyle
renderCmdFailure err =
  if isCmdNotAvailable
    then
      pretty ("Could not find executable: `" <> cmdName (cmdFailureCmd err) <> "`.")
        <> line
        <> line
        <> pretty ("Please ensure `" <> cmdName (cmdFailureCmd err) <> "` exist in PATH prior to running fossa.")
        <> line
        <> line
        <> reportDefectMsg
    else
      "Command execution failed: "
        <> line
        <> indent
          4
          ( vsep
              [ "command: " <> viaShow (cmdFailureCmd err)
              , "dir: " <> pretty (cmdFailureDir err)
              , "exit: " <> viaShow (cmdFailureExit err)
              , "stdout: " <> line <> indent 2 (pretty @Text (decodeUtf8 (cmdFailureStdout err)))
              , "stderr: " <> line <> indent 2 (pretty stdErr)
              ]
          )
        <> line
        <> reportDefectMsg
  where
    -- Infer if the stderr is caused by not having executable in path.
    -- There is no easy way to check for @EBADF@ within process exception,
    -- with the library we use and effort required.
    isCmdNotAvailable :: Bool
    isCmdNotAvailable = expectedCmdNotFoundErrStr == stdErr

    expectedCmdNotFoundErrStr :: Text
    expectedCmdNotFoundErrStr = cmdName (cmdFailureCmd err) <> ": startProcess: exec: invalid argument (Bad file descriptor)"

    stdErr :: Text
    stdErr = decodeUtf8 (cmdFailureStderr err)

instance ToDiagnostic ExecErr where
  renderDiagnostic = \case
    CommandFailed err -> renderCmdFailure err
    CommandParseError cmd err ->
      vsep
        [ "Failed to parse command output. command: " <> viaShow cmd <> "."
        , ""
        , indent 4 (pretty err)
        , ""
        , reportDefectMsg
        ]

-- | Execute a command and return its @(exitcode, stdout, stderr)@
exec :: Has Exec sig m => Path Abs Dir -> Command -> m (Either CmdFailure Stdout)
exec dir cmd = sendSimple (Exec (Abs dir) cmd Nothing)

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

-- | A variant of 'execThrow' that runs the command in the current directory
execThrow' :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => Command -> m BL.ByteString
execThrow' cmd = context ("Running command '" <> cmdName cmd <> "'") $ do
  dir <- getCurrentDir
  execThrow dir cmd

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
