{-# language TemplateHaskell #-}

module Effect.Exec
  ( Exec(..)
  , exec
  , execThrow
  , Command(..)
  , AllowErr(..)

  , execParser
  , execJson

  , execInputParser
  , execInputJson

  , execToIO
  , execConst
  , module System.Exit
  ) where

import Prologue

import           Control.Exception hiding (throw)
import           Control.Monad.Except (ExceptT(..), runExceptT)
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy.Optics
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Optics
import           Path.IO
import           Polysemy
import           Polysemy.Input
import           Polysemy.Error hiding (catch)
import           System.Exit (ExitCode(..))
import           System.Process.Typed
import           Text.Megaparsec (Parsec, runParser)
import           Text.Megaparsec.Error (errorBundlePretty)

import Diagnostics

data Command = Command
  { cmdNames    :: [String] -- ^ Possible command names. E.g., "pip", "pip3", "./gradlew".
  , cmdBaseArgs :: [String] -- ^ Base arguments for the command. Additional arguments can be passed when running commands (e.g., 'exec')
  , cmdAllowErr :: AllowErr -- ^ Error (i.e. non-zero exit code) tolerance policy for running commands. This is helpful for commands like @npm@, that nonsensically return non-zero exit codes when a command succeeds
  } deriving (Eq, Ord, Show, Generic)

data CmdFailure = CmdFailure
  { cmdFailureName   :: String
  , cmdFailureExit   :: ExitCode
  , cmdFailureStderr :: Stderr
  } deriving (Eq, Ord, Show, Generic)

data AllowErr =
    Never -- ^ never ignore non-zero exit (return 'CLIErr')
  | NonEmptyStdout -- ^ when `stdout` is non-empty, ignore non-zero exit
  | Always -- ^ always ignore non-zero exit
    deriving (Eq, Ord, Show, Generic)

type Stdout = BL.ByteString
type Stderr = BL.ByteString

data Exec m a where
  -- | Exec runs a command and returns either:
  -- - stdout when any of the 'cmdNames' succeed
  -- - failure descriptions for all of the commands we tried
  Exec :: Path Rel Dir -> Command -> [String] -> Exec m (Either [CmdFailure] Stdout)

makeSem_ ''Exec

-- | Execute a command and return its @(exitcode, stdout, stderr)@
exec :: Member Exec r => Path Rel Dir -> Command -> [String] -> Sem r (Either [CmdFailure] Stdout)

type Parser = Parsec Void Text

-- | Parse the stdout of a command
execParser :: Members '[Exec, Error CLIErr] r => Parser a -> Path Rel Dir -> Command -> [String] -> Sem r a
execParser parser dir cmd args = do
  stdout <- execThrow dir cmd args
  case runParser parser "" (TL.toStrict (decodeUtf8 stdout)) of
    Left err -> throw (CommandParseError "" (T.pack (errorBundlePretty err))) -- TODO: command name
    Right a -> pure a

-- | Parse the JSON stdout of a command
execJson :: (FromJSON a, Members '[Exec, Error CLIErr] r) => Path Rel Dir -> Command -> [String] -> Sem r a
execJson dir cmd args = do
  stdout <- execThrow dir cmd args
  case eitherDecode stdout of
    Left err -> throw (CommandParseError "" (T.pack (show err))) -- TODO: command name
    Right a -> pure a

-- | Interpret an 'Input' effect by parsing stdout of a command
execInputParser :: Members '[Exec, Error CLIErr] r => Parser i -> Path Rel Dir -> Command -> [String] -> Sem (Input i ': r) a -> Sem r a
execInputParser parser dir cmd args = interpret $ \case
  Input -> execParser parser dir cmd args
{-# INLINE execInputParser #-}

-- | Interpret an 'Input' effect by parsing JSON stdout of a command
execInputJson :: (FromJSON i, Members '[Exec, Error CLIErr] r) => Path Rel Dir -> Command -> [String] -> Sem (Input i ': r) a -> Sem r a
execInputJson dir cmd args = interpret $ \case
  Input -> execJson dir cmd args
{-# INLINE execInputJson #-}

-- | A variant of 'exec' that throws a 'CLIErr' when the command returns a non-zero exit code
execThrow :: Members '[Exec, Error CLIErr] r => Path Rel Dir -> Command -> [String] -> Sem r BL.ByteString
execThrow dir cmd args = do
  result <- exec dir cmd args
  case result of
    Left failures -> throw (CommandFailed "" (T.pack (show failures))) -- TODO: better error
    Right stdout -> pure stdout
{-# INLINE execThrow #-}

-- | Interpret an Exec effect by running commands
execToIO :: Member (Embed IO) r => Sem (Exec ': r) a -> Sem r a
execToIO = interpret $ \case
  Exec dir cmd args -> do
    absolute <- makeAbsolute dir
    -- TODO: disgusting/unreadable
    -- We use `ExceptT [CmdFailure] IO Stdout` here because it has the Alternative instance we want.
    --
    -- In particular: when all of the commands fail, we want to have descriptions of all of the
    -- CmdFailures, so we can produce better error messages.
    --
    -- A Command can have many `cmdNames`. ["./gradlew", "gradle"] is one such example.
    -- Each of them will be attempted successively until one succeeds
    --
    -- This is the behavior of `asum` with ExceptT: it'll run all of the ExceptT actions in the list, combining
    -- errors. When it finds a successful result, it'll return that instead of the accumulated errors
    let runCmd :: String -> ExceptT [CmdFailure] IO Stdout
        runCmd cmdName = ExceptT $ handle (\(e :: IOException) -> pure (Left [CmdFailure cmdName (ExitFailure (-1)) (show e ^. packedChars)])) $ do
          (exitcode, stdout, stderr) <- readProcess (setWorkingDir (fromAbsDir absolute) (proc cmdName (cmdBaseArgs cmd <> args)))
          case (exitcode, cmdAllowErr cmd) of
            (ExitSuccess, _) -> pure (Right stdout)
            (_, Never) -> pure (Left [CmdFailure cmdName exitcode stderr])
            (_, NonEmptyStdout) ->
              if BL.null stdout
                then pure (Left [CmdFailure cmdName exitcode stderr])
                else pure (Right stdout)
            (_, Always) -> pure (Right stdout)
    embed $ runExceptT $ asum (map runCmd (cmdNames cmd))
{-# INLINE execToIO #-}

-- | Interpret an Exec effect by providing a mock return value
execConst :: Either [CmdFailure] Stdout -> Sem (Exec ': r) a -> Sem r a
execConst out = interpret $ \case
  Exec _ _ _ -> pure out
{-# INLINE execConst #-}
