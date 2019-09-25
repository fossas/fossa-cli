
{-# language TemplateHaskell #-}

module Effect.ReadFS
  ( ReadFS(..)
  , readFSToIO

  , readContents
  , doesFileExist
  , doesDirExist
  ) where

import Prologue

import qualified Data.ByteString as BS
import           Path (Dir, File, Path, toFilePath)
import qualified Path.IO as PIO
import           Polysemy

data ReadFS m a where
  ReadContents   :: Path b File -> ReadFS m ByteString
  DoesFileExist  :: Path b File -> ReadFS m Bool
  DoesDirExist   :: Path b Dir  -> ReadFS m Bool

readFSToIO :: Member (Embed IO) r => InterpreterFor ReadFS r
readFSToIO = interpret $ \case
  ReadContents file -> embed $ BS.readFile (toFilePath file)
  DoesFileExist file -> PIO.doesFileExist file
  DoesDirExist dir -> PIO.doesDirExist dir
{-# INLINE readFSToIO #-}

makeSem_ ''ReadFS

-- | Read a file from the filesystem
readContents  :: Member ReadFS r => Path b File -> Sem r ByteString
doesFileExist :: Member ReadFS r => Path b File -> Sem r Bool
doesDirExist  :: Member ReadFS r => Path b Dir  -> Sem r Bool
