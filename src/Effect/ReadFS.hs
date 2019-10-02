
{-# language TemplateHaskell #-}

module Effect.ReadFS
  ( ReadFS(..)
  , readFSToIO

  , readContentsBS
  , readContentsText
  , doesFileExist
  , doesDirExist
  ) where

import Prologue

import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO
import           Path (Dir, File, Path, toFilePath)
import qualified Path.IO as PIO
import           Polysemy

data ReadFS m a where
  ReadContentsBS   :: Path b File -> ReadFS m ByteString
  ReadContentsText :: Path b File -> ReadFS m Text
  DoesFileExist    :: Path b File -> ReadFS m Bool
  DoesDirExist     :: Path b Dir  -> ReadFS m Bool

makeSem ''ReadFS

readFSToIO :: Member (Embed IO) r => InterpreterFor ReadFS r
readFSToIO = interpret $ \case
  ReadContentsBS file -> embed $ BS.readFile (toFilePath file)
  ReadContentsText file -> embed $ TIO.readFile (toFilePath file)
  DoesFileExist file -> PIO.doesFileExist file
  DoesDirExist dir -> PIO.doesDirExist dir
{-# INLINE readFSToIO #-}
