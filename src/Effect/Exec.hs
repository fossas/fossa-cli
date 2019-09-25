{-# language TemplateHaskell #-}

module Effect.Exec
  ( Exec(..)
  , exec
  , execToIO
  ) where

import Prologue

import qualified Data.ByteString.Lazy as BL
import           Path.IO
import           Polysemy
import           System.Exit
import           System.Process.Typed

data Exec m a where
  Exec :: Path Rel Dir -> String -> [String] -> Exec m (ExitCode, BL.ByteString, BL.ByteString) -- stdout, stderr

execToIO :: Member (Embed IO) r => InterpreterFor Exec r
execToIO = interpret $ \case
  Exec dir cmd args -> do
    absolute <- makeAbsolute dir
    readProcess (setWorkingDir (toFilePath absolute) (proc cmd args))
{-# INLINE execToIO #-}

makeSem ''Exec
