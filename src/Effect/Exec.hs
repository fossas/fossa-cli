{-# language TemplateHaskell #-}

module Effect.Exec
  ( Exec(..)
  , exec
  , execToIO
  , module System.Exit
  ) where

import Prologue

import           Control.Exception
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy.Optics
import           Optics
import           Path.IO
import           Polysemy
import           System.Exit (ExitCode(..))
import           System.Process.Typed

data Exec m a where
  Exec :: Path Rel Dir -> String -> [String] -> Exec m (ExitCode, BL.ByteString, BL.ByteString) -- stdout, stderr

execToIO :: Member (Embed IO) r => InterpreterFor Exec r
execToIO = interpret $ \case
  Exec dir cmd args -> do
    absolute <- makeAbsolute dir
    embed @IO $
      readProcess (setWorkingDir (toFilePath absolute) (proc cmd args))
        `catch` (\(e :: IOException) -> pure (ExitFailure (-1), "", show e ^. packedChars)) -- TODO: better error?
{-# INLINE execToIO #-}

makeSem ''Exec
