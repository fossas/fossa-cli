module Effect.Exec
  ( Exec (..),
    ExecErr (..),
    exec,
    execThrow,
    Command (..),
    AllowErr (..),
    execParser,
    execJson,
    ExecIOC (..),
    runExecIO,
    module System.Exit,
    module X,
  )
where

import Control.Algebra as X
import Control.Carrier.Error.Either
import qualified Control.Exception as Exc
import Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Optics
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Optics
import Path.IO
import Prologue
import System.Exit (ExitCode (..))
import System.Process.Typed
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Error (errorBundlePretty)

data Command = Command
  { -- | Possible command names. E.g., "pip", "pip3", "./gradlew".
    cmdNames :: [String],
    -- | Base arguments for the command. Additional arguments can be passed when running commands (e.g., 'exec')
    cmdBaseArgs :: [String],
    -- | Error (i.e. non-zero exit code) tolerance policy for running commands. This is helpful for commands like @npm@, that nonsensically return non-zero exit codes when a command succeeds
    cmdAllowErr :: AllowErr
  }
  deriving (Eq, Ord, Show, Generic)

data CmdFailure = CmdFailure
  { cmdFailureName :: String,
    cmdFailureExit :: ExitCode,
    cmdFailureStderr :: Stderr
  }
  deriving (Eq, Ord, Show, Generic)

data AllowErr
  = -- | never ignore non-zero exit (return 'ExecErr')
    Never
  | -- | when `stdout` is non-empty, ignore non-zero exit
    NonEmptyStdout
  | -- | always ignore non-zero exit
    Always
  deriving (Eq, Ord, Show, Generic)

type Stdout = BL.ByteString
type Stderr = BL.ByteString

data Exec m k where
  -- | Exec runs a command and returns either:
  -- - stdout when any of the 'cmdNames' succeed
  -- - failure descriptions for all of the commands we tried

  Exec :: Path x Dir -> Command -> [String] -> Exec m (Either [CmdFailure] Stdout)

-- TODO: include info about CmdFailures as appropriate
data ExecErr
  = -- | Command execution failed, usually from a non-zero exit. command, stderr
    CommandFailed Text Text
  | -- | Command output couldn't be parsed. command, err
    CommandParseError Text Text
  deriving (Eq, Ord, Show, Generic, Typeable)

instance Exc.Exception ExecErr

-- | Execute a command and return its @(exitcode, stdout, stderr)@
exec :: Has Exec sig m => Path x Dir -> Command -> [String] -> m (Either [CmdFailure] Stdout)
exec dir cmd args = send (Exec dir cmd args)

type Parser = Parsec Void Text

-- | Parse the stdout of a command
execParser :: (Has Exec sig m, Has (Error ExecErr) sig m) => Parser a -> Path x Dir -> Command -> [String] -> m a
execParser parser dir cmd args = do
  stdout <- execThrow dir cmd args
  case runParser parser "" (TL.toStrict (decodeUtf8 stdout)) of
    Left err -> throwError (CommandParseError "" (T.pack (errorBundlePretty err))) -- TODO: command name
    Right a -> pure a

-- | Parse the JSON stdout of a command
execJson :: (FromJSON a, Has Exec sig m, Has (Error ExecErr) sig m) => Path x Dir -> Command -> [String] -> m a
execJson dir cmd args = do
  stdout <- execThrow dir cmd args
  case eitherDecode stdout of
    Left err -> throwError (CommandParseError "" (T.pack (show err))) -- TODO: command name
    Right a -> pure a

-- | A variant of 'exec' that throws a 'ExecErr' when the command returns a non-zero exit code
execThrow :: (Has Exec sig m, Has (Error ExecErr) sig m) => Path x Dir -> Command -> [String] -> m BL.ByteString
execThrow dir cmd args = do
  result <- exec dir cmd args
  case result of
    Left failures -> throwError (CommandFailed "" (T.pack (show failures))) -- TODO: better error
    Right stdout -> pure stdout
{-# INLINE execThrow #-}

runExecIO :: ExecIOC m a -> m a
runExecIO = runExecIOC

newtype ExecIOC m a = ExecIOC {runExecIOC :: m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

instance (Algebra sig m, MonadIO m) => Algebra (Exec :+: sig) (ExecIOC m) where
  alg hdl sig ctx = ExecIOC $ case sig of
    R other -> alg (runExecIOC . hdl) other ctx
    L (Exec dir cmd args) -> liftIO $ do
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
          runCmd cmdName = ExceptT $ Exc.handle (\(e :: Exc.IOException) -> pure (Left [CmdFailure cmdName (ExitFailure (-1)) (show e ^. packedChars)])) $ do
            (exitcode, stdout, stderr) <- readProcess (setWorkingDir (fromAbsDir absolute) (proc cmdName (cmdBaseArgs cmd <> args)))
            case (exitcode, cmdAllowErr cmd) of
              (ExitSuccess, _) -> pure (Right stdout)
              (_, Never) -> pure (Left [CmdFailure cmdName exitcode stderr])
              (_, NonEmptyStdout) ->
                if BL.null stdout
                  then pure (Left [CmdFailure cmdName exitcode stderr])
                  else pure (Right stdout)
              (_, Always) -> pure (Right stdout)
      res <- runExceptT $ asum (map runCmd (cmdNames cmd))
      pure (res <$ ctx)
