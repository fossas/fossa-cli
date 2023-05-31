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
  WhichEffs,
  which,
  which',
  FoundLocation (..),
  foundLocationToCommand,
) where

import App.Support (reportDefectMsg)
import App.Types (SystemPath (unSystemPath), SystemPathExt (..))
import App.Util (SupportedOS (Windows), runningInOS)
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
  fatalOnIOException,
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
import Data.List.NonEmpty (NonEmpty, singleton)
import Data.List.NonEmpty qualified as NE
import Data.String (fromString)
import Data.String.Conversion (decodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Effect.ReadFS (ReadFS, doesFileExist, getCurrentDir)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, SomeBase (..), fromAbsDir, parent, parseRelFile, (</>))
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
      <$> obj
      .: "cmdFailureCmd"
      <*> obj
      .: "cmdFailureDir"
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

renderCmdFailure :: CmdFailure -> Doc AnsiStyle
renderCmdFailure CmdFailure{..} =
  if isCmdNotAvailable
    then
      vsep
        [ pretty $ "Could not find executable: `" <> cmdName cmdFailureCmd <> "`."
        , pretty $ "Please ensure `" <> cmdName cmdFailureCmd <> "` exists in PATH prior to running fossa."
        , ""
        , reportDefectMsg
        ]
    else
      vsep
        [ "Command execution failed: "
        , ""
        , indent 4 details
        , ""
        , reportDefectMsg
        ]
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
  renderDiagnostic = \case
    ExecEnvNotSupported env -> pretty $ "Exec is not supported in: " <> env
    CommandFailed err -> renderCmdFailure err
    CommandParseError cmd err ->
      vsep
        [ "Failed to parse command output. command: " <> viaShow cmd <> "."
        , ""
        , indent 4 (pretty err)
        , ""
        , reportDefectMsg
        ]

-- | It is recommended that, if using @which@ or @which'@ to execute a binary,
-- binaries accessible in the system path are called simply by their name
-- on supported platforms.
--
-- This is because we've seen issues in some platforms where users have
-- a binary in their path and we provide the full path to the binary
-- the shell fails to run it, but if we provide just the name it works.
--
-- This type provides the ability to disambiguate:
-- the @FoundInSystemPath@ case provides the path but additionally provides
-- the argument that is recommended to pass as the @commandName@ in a @Command@.
data FoundLocation
  = FoundInWorkDir (Path Abs File)
  | FoundInSystemPath (Path Abs File) Text

-- | Convert a @FoundLocation@ to a @Command@ directly with the provided options.
foundLocationToCommand :: [Text] -> AllowErr -> FoundLocation -> Command
foundLocationToCommand args err = \case
  FoundInWorkDir path -> Command (toText path) args err
  FoundInSystemPath _ cmdName -> Command cmdName args err

-- | Effects used for @which@ and @which'@.
type WhichEffs sig m =
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader SystemPath) sig m
  , Has (Reader SystemPathExt) sig m
  , Has ReadFS sig m
  )

-- | Find the full path to an executable on the system.
--
-- The function searches for the binary with the first found of @Extensions@,
-- in the first of provided @Directories@.
-- The search is conducted in the order of directories -> extensions.
--
-- If multiple binaries are intended, see @which'@.
--
-- If all directories and extensions are exhausted without finding a match,
-- the function returns @Nothing@.
--
-- * @Directories@
-- The function begins its search in the provided working directory and all of its parent directories.
-- After that, it searches all entries in the system @$PATH@ environment variable.
--
-- * @Extensions@
-- In Unix-based systems, @Extensions@ is always an empty list.
-- In Windows-based systems, the function considers all entries in the system's
-- @$PATHEXT@ environment variable.
--
-- * Examples:
--
-- __Windows__
--
-- > Binary : "mvn"
-- > PATH   : "C:\System32"
-- > PATHEXT: ".bat;.exe"
-- > WORKDIR: "C:\Users\me\projects\example"
--
-- Searches:
--
-- > C:\Users\me\projects\example\mvn.exe
-- > C:\Users\me\projects\example\mvn.bat
-- > C:\Users\me\projects\mvn.exe
-- > C:\Users\me\projects\mvn.bat
-- > C:\Users\me\mvn.exe
-- > C:\Users\me\mvn.bat
-- > C:\Users\mvn.exe
-- > C:\Users\mvn.bat
-- > C:\mvn.exe
-- > C:\mvn.bat
-- > C:\System32\mvn.exe
-- > C:\System32\mvn.bat
--
-- __Linux, macOS__
--
-- > Binary : "mvn"
-- > PATH   : "/usr/local/bin"
-- > WORKDIR: "/home/me/projects/example"
--
-- Searches:
--
-- > /home/me/projects/example/mvn
-- > /home/me/projects/mvn
-- > /home/me/mvn
-- > /home/mvn
-- > /mvn
-- > /usr/local/bin/mvn
which :: (WhichEffs sig m) => Path Abs Dir -> Text -> m (Maybe FoundLocation)
which workdir bin = which' workdir (singleton bin)

-- | Find the full path to an executable on the system.
--
-- The function searches for the first found in the list of @Binaries@,
-- with the first found of @Extensions@, in the first of provided @Directories@.
-- The search is conducted in the order of directories -> binaries -> extensions.
--
-- If all directories, binaries, and extensions are exhausted without finding a match,
-- the function returns @Nothing@.
--
-- * @Directories@
-- The function begins its search in the provided working directory and all of its parent directories.
-- After that, it searches all entries in the system @$PATH@ environment variable.
--
-- * @Extensions@
-- In Unix-based systems, @Extensions@ is always an empty list.
-- In Windows-based systems, the function considers all entries in the system's
-- @$PATHEXT@ environment variable.
--
-- * @Binaries@
-- The function can be provided with multiple binary names.
-- This is useful in cases where a binary might have different names,
-- but the usage of the binary is exactly the same regardless of the name used.
-- The binary names are searched in order, and the function returns the path of the first one found.
--
-- * Examples:
--
-- __Windows__
--
-- > Binaries: [ "mvn", "mvnw" ]
-- > PATH    : "C:\System32"
-- > PATHEXT : ".bat;.exe"
-- > WORKDIR : "C:\Users\me\projects\example"
--
-- Searches:
--
-- > C:\Users\me\projects\example\mvn.exe
-- > C:\Users\me\projects\example\mvn.bat
-- > C:\Users\me\projects\example\mvnw.exe
-- > C:\Users\me\projects\example\mvnw.bat
-- > C:\Users\me\projects\mvn.exe
-- > C:\Users\me\projects\mvn.bat
-- > C:\Users\me\projects\mvnw.exe
-- > C:\Users\me\projects\mvnw.bat
-- > C:\Users\me\mvn.exe
-- > C:\Users\me\mvn.bat
-- > C:\Users\me\mvnw.exe
-- > C:\Users\me\mvnw.bat
-- > C:\Users\mvn.exe
-- > C:\Users\mvn.bat
-- > C:\Users\mvnw.exe
-- > C:\Users\mvnw.bat
-- > C:\mvn.exe
-- > C:\mvn.bat
-- > C:\mvnw.exe
-- > C:\mvnw.bat
-- > C:\System32\mvn.exe
-- > C:\System32\mvn.bat
-- > C:\System32\mvnw.exe
-- > C:\System32\mvnw.bat
--
-- __Linux, macOS__
--
-- > Binaries: ["mvn", "mvnw"]
-- > PATH    : "/usr/local/bin"
-- > WORKDIR : "/home/me/projects/example"
--
-- Searches:
--
-- > /home/me/projects/example/mvn
-- > /home/me/projects/example/mvnw
-- > /home/me/projects/mvn
-- > /home/me/projects/mvnw
-- > /home/me/mvn
-- > /home/me/mvnw
-- > /home/mvn
-- > /home/mvnw
-- > /mvn
-- > /mvnw
-- > /usr/local/bin/mvn
-- > /usr/local/bin/mvnw
which' :: (WhichEffs sig m) => Path Abs Dir -> NonEmpty Text -> m (Maybe FoundLocation)
which' workdir bins = context describe $ do
  (systemPaths :: SystemPath) <- ask
  (systemPathExts :: SystemPathExt) <- ask

  let names = NE.toList bins
      exts = unSystemPathExt systemPathExts
      workdirPaths = enumerateWithParents workdir
      systemPaths' = unSystemPath systemPaths

  context "find in workdir ancestors" $
    nextPath workdirPaths names exts >>= \case
      Just (path, _) -> pure . Just $ FoundInWorkDir path
      Nothing ->
        context "find in system path" $
          nextPath systemPaths' names exts >>= \case
            Just (path, name) -> pure . Just $ systemPathFoundLocation (path, name)
            Nothing -> pure Nothing
  where
    describe :: Text
    describe =
      toText $
        concat
          [ "which' { binaries: "
          , show bins
          , ", target workdir: "
          , show workdir
          , " }"
          ]

    systemPathFoundLocation :: (Path Abs File, Text) -> FoundLocation
    systemPathFoundLocation (path, cmdName) | runningInOS Windows = FoundInSystemPath path cmdName
    systemPathFoundLocation (path, _) = FoundInSystemPath path $ toText path

    enumerateWithParents :: Path Abs Dir -> [Path Abs Dir]
    enumerateWithParents path = do
      let next = parent path
      if next /= path
        then ([next] ++) $ enumerateWithParents next
        else [next]

    nextPath :: (WhichEffs sig m) => [Path Abs Dir] -> [Text] -> [String] -> m (Maybe (Path Abs File, Text))
    nextPath (path : paths) names exts = do
      found <- nextName path names exts
      case found of
        Just file -> pure $ Just file
        Nothing -> nextPath paths names exts
    nextPath [] _ _ = pure Nothing

    nextName :: (WhichEffs sig m) => Path Abs Dir -> [Text] -> [String] -> m (Maybe (Path Abs File, Text))
    nextName path (name : names) exts = do
      found <- nextExt path name exts
      case found of
        Just file -> pure $ Just file
        Nothing -> nextName path names exts
    nextName _ [] _ = pure Nothing

    nextExt :: (WhichEffs sig m) => Path Abs Dir -> Text -> [String] -> m (Maybe (Path Abs File, Text))
    nextExt path name (ext : exts) = do
      file <-
        (fatalOnIOException "parse constructed candidate") . sendIO . parseRelFile $
          toString name <> ext
      let candidate = path </> file
      exists <- doesFileExist candidate
      if exists
        then pure $ Just (candidate, name)
        else nextExt path name exts
    nextExt _ _ [] = pure Nothing

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
