{-# language TemplateHaskell #-}

module Effect.Exec
  ( Exec(..)
  , exec
  , execThrow

  , execToIO
  , module System.Exit
  ) where

import Prologue

import           Control.Exception hiding (throw)
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy.Optics
import           Optics
import           Path.IO
import           Polysemy
import           Polysemy.Error hiding (catch)
import           System.Exit (ExitCode(..))
import           System.Process.Typed

import Diagnostics

data Exec m a where
  Exec :: Path Rel Dir -> String -> [String] -> Exec m (ExitCode, BL.ByteString, BL.ByteString) -- stdout, stderr

makeSem_ ''Exec

-- | Execute a command and return its @(exitcode, stdout, stderr)@
exec :: Member Exec r => Path Rel Dir -> String -> [String] -> Sem r (ExitCode, BL.ByteString, BL.ByteString)

-- | A variant of 'exec' that throws a 'CLIErr' when the command returns a non-zero exit code
execThrow :: Members '[Exec, Error CLIErr] r => Path Rel Dir -> String -> [String] -> Sem r BL.ByteString
execThrow dir cmd args = do
  (exitcode, stdout, stderr) <- exec dir cmd args
  when (exitcode /= ExitSuccess) $ throw (CommandFailed cmd (stderr ^. unpackedChars))
  pure stdout

execToIO :: Member (Embed IO) r => InterpreterFor Exec r
execToIO = interpret $ \case
  Exec dir cmd args -> do
    absolute <- makeAbsolute dir
    embed $ readProcess (setWorkingDir (fromAbsDir absolute) (proc cmd args))
      `catch` (\(e :: IOException) -> pure (ExitFailure (-1), "", show e ^. packedChars)) -- TODO: better error?
{-# INLINE execToIO #-}

