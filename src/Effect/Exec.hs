{-# language TemplateHaskell #-}

module Effect.Exec
  ( Exec(..)
  , exec
  , execThrow
  , Command(..)

  , execInputParser
  , execInputJson

  , execToIO
  , module System.Exit
  ) where

import Prologue

import           Control.Exception hiding (throw)
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy.Optics
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

import Diagnostics

data Command = Command
  { cmdNames :: [Path Rel File] -- ^ Possible command names. E.g., @[[relfile|pip|], [relfile|pip3|]]@. This may also include things like @[relfile|gradlew|]@
  , cmdArgs  :: [String]
  }

data Exec m a where
  Exec :: Path Rel Dir -> Command -> Exec m (ExitCode, BL.ByteString, BL.ByteString) -- stdout, stderr

makeSem_ ''Exec

-- | Execute a command and return its @(exitcode, stdout, stderr)@
exec :: Member Exec r => Path Rel Dir -> Command -> Sem r (ExitCode, BL.ByteString, BL.ByteString)

type Parser = Parsec Void Text

execInputParser :: Members '[Exec, Error CLIErr] r => Parser a -> Path Rel Dir -> Command -> InterpreterFor (Input a) r
execInputParser parser dir cmd = interpret $ \case
  Input -> do
    stdout <- execThrow dir cmd
    case runParser parser "" (TL.toStrict (decodeUtf8 stdout)) of
      Left err -> throw (CommandParseError "" (show err)) -- TODO: command name
      Right a -> pure a
{-# INLINE execInputParser #-}

execInputJson :: (FromJSON a, Members '[Exec, Error CLIErr] r) => Path Rel Dir -> Command -> InterpreterFor (Input a) r
execInputJson dir cmd = interpret $ \case
  Input -> do
    stdout <- execThrow dir cmd
    case eitherDecode stdout of
      Left err -> throw (CommandParseError "" (show err)) -- TODO: command name
      Right a -> pure a
{-# INLINE execInputJson #-}

-- | A variant of 'exec' that throws a 'CLIErr' when the command returns a non-zero exit code
execThrow :: Members '[Exec, Error CLIErr] r => Path Rel Dir -> Command -> Sem r BL.ByteString
execThrow dir cmd = do
  (exitcode, stdout, stderr) <- exec dir cmd
  when (exitcode /= ExitSuccess) $ throw (CommandFailed "" (stderr ^. unpackedChars)) -- TODO: command name
  pure stdout

execToIO :: Member (Embed IO) r => InterpreterFor Exec r
execToIO = interpret $ \case
  Exec dir cmd -> do
    absolute <- makeAbsolute dir
    embed $ asum (map (\cmdName -> readProcess (setWorkingDir (fromAbsDir absolute) (proc cmdName (cmdArgs cmd)))) (map fromRelFile (cmdNames cmd)))
      `catch` (\(e :: IOException) -> pure (ExitFailure (-1), "", show e ^. packedChars)) -- TODO: better error?
{-# INLINE execToIO #-}

