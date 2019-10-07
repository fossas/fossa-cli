{-# language TemplateHaskell #-}

module Effect.Exec
  ( Exec(..)
  , exec
  , tryExec
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
  Exec    :: Path Rel Dir -> String -> [String] -> Exec m BL.ByteString -- stdout
  TryExec :: Path Rel Dir -> String -> [String] -> Exec m (ExitCode, BL.ByteString, BL.ByteString) -- stdout, stderr

runCmd :: Path Rel Dir -> String -> [String] -> IO (ExitCode, BL.ByteString, BL.ByteString)
runCmd dir cmd args = do
  absolute <- makeAbsolute dir
  readProcess (setWorkingDir (fromAbsDir absolute) (proc cmd args))
    `catch` (\(e :: IOException) -> pure (ExitFailure (-1), "", show e ^. packedChars)) -- TODO: better error?

execToIO :: Members '[Embed IO, Error CLIErr] r => InterpreterFor Exec r
execToIO = interpret $ \case
  Exec dir cmd args -> do
    (exitcode, stdout, stderr) <- embed $ runCmd dir cmd args
    when (exitcode /= ExitSuccess) $ throw (CommandFailed cmd (stderr ^. unpackedChars))
    pure stdout
  TryExec dir cmd args -> do
    embed $ runCmd dir cmd args
{-# INLINE execToIO #-}

makeSem ''Exec
