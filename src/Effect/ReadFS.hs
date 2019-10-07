
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

import           Control.Exception
import qualified Data.ByteString as BS
import           Data.Text.Encoding (decodeUtf8)
import           Path (Dir, File, Path, toFilePath)
import qualified Path.IO as PIO
import           Polysemy
import           Polysemy.Error hiding (catch)

import Diagnostics

data ReadFS m a where
  ReadContentsBS   :: Path b File -> ReadFS m ByteString
  ReadContentsText :: Path b File -> ReadFS m Text
  DoesFileExist    :: Path b File -> ReadFS m Bool
  DoesDirExist     :: Path b Dir  -> ReadFS m Bool

makeSem ''ReadFS

readFSToIO :: Members '[Embed IO, Error CLIErr] r => InterpreterFor ReadFS r
readFSToIO = interpret $ \case
  ReadContentsBS file -> fromEitherM $
    (Right <$> BS.readFile (toFilePath file))
    `catch`
    (\(e :: IOException) -> pure (Left (FileReadError (show e))))
  ReadContentsText file -> fromEitherM $
    (Right . decodeUtf8 <$> BS.readFile (toFilePath file))
    `catch`
    (\(e :: IOException) -> pure (Left (FileReadError (show e))))
  DoesFileExist file -> PIO.doesFileExist file
  DoesDirExist dir -> PIO.doesDirExist dir
{-# INLINE readFSToIO #-}
