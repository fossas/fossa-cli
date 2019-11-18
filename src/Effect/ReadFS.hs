
{-# language TemplateHaskell #-}

module Effect.ReadFS
  ( ReadFS(..)
  , readFSToIO

  , readContentsBS
  , readContentsText
  , doesFileExist
  , doesDirExist

  , readContentsParser
  , readContentsJson

  , fileInputParser
  , fileInputJson
  ) where

import Prologue

import           Control.Exception hiding (throw)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Path (Dir, File, Path, toFilePath)
import qualified Path.IO as PIO
import           Polysemy
import           Polysemy.Error hiding (catch)
import           Polysemy.Input
import           Text.Megaparsec (Parsec, runParser)
import           Text.Megaparsec.Error (errorBundlePretty)

import Diagnostics

data ReadFS m a where
  ReadContentsBS   :: Path b File -> ReadFS m ByteString
  ReadContentsText :: Path b File -> ReadFS m Text
  DoesFileExist    :: Path b File -> ReadFS m Bool
  DoesDirExist     :: Path b Dir  -> ReadFS m Bool

makeSem_ ''ReadFS

-- | Read file contents into a strict 'ByteString'
readContentsBS :: Member ReadFS r => Path b File -> Sem r ByteString

-- | Read file contents into a strict 'Text'
readContentsText :: Member ReadFS r => Path b File -> Sem r Text

-- | Check whether a file exists
doesFileExist :: Member ReadFS r => Path b File -> Sem r Bool

-- | Check whether a directory exists
doesDirExist :: Member ReadFS r => Path b Dir -> Sem r Bool

type Parser = Parsec Void Text

-- | Read from a file, parsing its contents
readContentsParser :: Members '[ReadFS, Error ReadFSErr] r => Parser a -> Path b File -> Sem r a
readContentsParser parser file = do
  contents <- readContentsText file
  case runParser parser (toFilePath file) contents of
    Left err -> throw (FileParseError (toFilePath file) (T.pack (errorBundlePretty err)))
    Right a -> pure a

-- | Read JSON from a file
readContentsJson :: (FromJSON a, Members '[ReadFS, Error ReadFSErr] r) => Path b File -> Sem r a
readContentsJson file = do
  contents <- readContentsBS file
  case eitherDecodeStrict contents of
    Left err -> throw (FileParseError (toFilePath file) (T.pack err))
    Right a -> pure a

-- | Interpret an 'Input' effect by parsing file contents
fileInputParser :: Members '[ReadFS, Error ReadFSErr] r => Parser i -> Path b File -> Sem (Input i ': r) a -> Sem r a
fileInputParser parser file = interpret $ \case
  Input -> readContentsParser parser file
{-# INLINE fileInputParser #-}

-- | Interpret an 'Input' effect by parsing JSON file contents
fileInputJson :: (FromJSON i, Members '[ReadFS, Error ReadFSErr] r) => Path b File -> Sem (Input i ': r) a -> Sem r a
fileInputJson file = interpret $ \case
  Input -> readContentsJson file
{-# INLINE fileInputJson #-}

readFSToIO :: Members '[Embed IO, Error ReadFSErr] r => Sem (ReadFS ': r) a -> Sem r a
readFSToIO = interpret $ \case
  ReadContentsBS file -> fromEitherM $
    (Right <$> BS.readFile (toFilePath file))
    `catch`
    (\(e :: IOException) -> pure (Left (FileReadError (toFilePath file) (T.pack (show e)))))
  ReadContentsText file -> fromEitherM $
    (Right . decodeUtf8 <$> BS.readFile (toFilePath file))
    `catch`
    (\(e :: IOException) -> pure (Left (FileReadError (toFilePath file) (T.pack (show e)))))
  DoesFileExist file -> PIO.doesFileExist file
  DoesDirExist dir -> PIO.doesDirExist dir
{-# INLINE readFSToIO #-}
