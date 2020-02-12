
{-# language TemplateHaskell #-}

module Effect.ReadFS
  ( -- * ReadFS Effect
    ReadFS(..)
  , readFSToIO

  -- * Reading raw file contents
  , readContentsBS
  , readContentsText

  -- * Resolving relative filepaths
  , resolveFile
  , resolveFile'
  , resolveDir
  , resolveDir'

  -- * Checking whether files exist
  , doesFileExist
  , doesDirExist

  -- * Parsing file contents
  , readContentsParser
  , readContentsParser'
  , readContentsJson
  , readContentsJson'
  , readContentsYaml
  , readContentsYaml'
  , readContentsXML
  , readContentsXML'

  -- * 'Input' interpreters
  , fileInputParser
  , fileInputJson
  , fileInputYaml
  , fileInputXML
  ) where

import Prologue

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Yaml (decodeEither', prettyPrintParseException)
import           Parse.XML (FromXML, parseXML, xmlErrorPretty)
import qualified Path.IO as PIO
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import           Text.Megaparsec (Parsec, runParser)
import           Text.Megaparsec.Error (errorBundlePretty)

import Diagnostics

data ReadFS m a where
  ReadContentsBS'   :: Path b File -> ReadFS m (Either ReadFSErr ByteString)
  ReadContentsText' :: Path b File -> ReadFS m (Either ReadFSErr Text)
  DoesFileExist     :: Path b File -> ReadFS m Bool
  DoesDirExist      :: Path b Dir  -> ReadFS m Bool
  ResolveFile'      :: Path Abs Dir -> Text -> ReadFS m (Either ReadFSErr (Path Abs File))
  ResolveDir'       :: Path Abs Dir -> Text -> ReadFS m (Either ReadFSErr (Path Abs Dir))

makeSem_ ''ReadFS

-- | Read file contents into a strict 'ByteString'
readContentsBS' :: Member ReadFS r => Path b File -> Sem r (Either ReadFSErr ByteString)

-- | Read file contents into a strict 'ByteString'
readContentsBS :: Members '[ReadFS, Error ReadFSErr] r => Path b File -> Sem r ByteString
readContentsBS = fromEither <=< readContentsBS'

-- | Read file contents into a strict 'Text'
readContentsText' :: Member ReadFS r => Path b File -> Sem r (Either ReadFSErr Text)

-- | Read file contents into a strict 'Text'
readContentsText :: Members '[ReadFS, Error ReadFSErr] r => Path b File -> Sem r Text
readContentsText = fromEither <=< readContentsText'

-- | Resolve a relative filepath to a file
resolveFile' :: Member ReadFS r => Path Abs Dir -> Text -> Sem r (Either ReadFSErr (Path Abs File))

-- | Resolve a relative filepath to a file
resolveFile :: Members '[ReadFS, Error ReadFSErr] r => Path Abs Dir -> Text -> Sem r (Path Abs File)
resolveFile dir path = fromEither =<< resolveFile' dir path

-- | Resolve a relative filepath to a directory
resolveDir' :: Member ReadFS r => Path Abs Dir -> Text -> Sem r (Either ReadFSErr (Path Abs Dir))

-- | Resolve a relative filepath to a directory
resolveDir :: Members '[ReadFS, Error ReadFSErr] r => Path Abs Dir -> Text -> Sem r (Path Abs Dir)
resolveDir dir path = fromEither =<< resolveDir' dir path

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

-- | Read from a file, parsing its contents
readContentsParser' :: Member ReadFS r => Parser a -> Path b File -> Sem r (Either ReadFSErr a)
readContentsParser' parser file = runError $ readContentsParser parser file

-- | Read JSON from a file
readContentsJson :: (FromJSON a, Members '[ReadFS, Error ReadFSErr] r) => Path b File -> Sem r a
readContentsJson file = do
  contents <- readContentsBS file
  case eitherDecodeStrict contents of
    Left err -> throw (FileParseError (toFilePath file) (T.pack err))
    Right a -> pure a

-- | Read JSON from a file
readContentsJson' :: (FromJSON a, Member ReadFS r) => Path b File -> Sem r (Either ReadFSErr a)
readContentsJson' = runError . readContentsJson

-- | Read YAML from a file
readContentsYaml :: (FromJSON a, Members '[ReadFS, Error ReadFSErr] r) => Path b File -> Sem r a
readContentsYaml file = do
  contents <- readContentsBS file
  case decodeEither' contents of
    Left err -> throw (FileParseError (toFilePath file) (T.pack$ prettyPrintParseException err))
    Right a -> pure a

-- | Read YAML from a file
readContentsYaml' :: (FromJSON a, Member ReadFS r) => Path b File -> Sem r (Either ReadFSErr a)
readContentsYaml' = runError . readContentsYaml

-- | Read XML from a file
readContentsXML :: (FromXML a, Members '[ReadFS, Error ReadFSErr] r) => Path b File -> Sem r a
readContentsXML file = do
  contents <- readContentsText file
  case parseXML contents of
    Left err -> throw (FileParseError (toFilePath file) (xmlErrorPretty err))
    Right a -> pure a

-- | Read XML from a file
readContentsXML' :: (FromXML a, Member ReadFS r) => Path b File -> Sem r (Either ReadFSErr a)
readContentsXML' = runError . readContentsXML

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

-- | Interpret an 'Input' effect by parsing YAML file contents
fileInputYaml :: (FromJSON i, Members '[ReadFS, Error ReadFSErr] r) => Path b File -> Sem (Input i ': r) a -> Sem r a
fileInputYaml file = interpret $ \case
  Input -> readContentsYaml file
{-# INLINE fileInputYaml #-}

-- | Interpret an 'Input' effect by parsing XML file contents
fileInputXML :: (FromXML i, Members '[ReadFS, Error ReadFSErr] r) => Path b File -> Sem (Input i ': r) a -> Sem r a
fileInputXML file = interpret $ \case
  Input -> readContentsXML file
{-# INLINE fileInputXML #-}

readFSToIO :: Members '[Embed IO] r => Sem (ReadFS ': r) a -> Sem r a
readFSToIO = interpret $ \case
  ReadContentsBS' file -> embed $
    (Right <$> BS.readFile (toFilePath file))
    `E.catch`
    (\(e :: E.IOException) -> pure (Left (FileReadError (toFilePath file) (T.pack (show e)))))
  ReadContentsText' file -> embed $
    (Right . decodeUtf8 <$> BS.readFile (toFilePath file))
    `E.catch`
    (\(e :: E.IOException) -> pure (Left (FileReadError (toFilePath file) (T.pack (show e)))))
  ResolveFile' dir path -> embed $
    (Right <$> PIO.resolveFile dir (T.unpack path))
    `E.catch`
    (\(e :: E.IOException) -> pure (Left (ResolveError (T.pack (show e)))))
  ResolveDir' dir path -> embed $
    (Right <$> PIO.resolveDir dir (T.unpack path))
    `E.catch`
    (\(e :: E.IOException) -> pure (Left (ResolveError (T.pack (show e)))))
  -- NB: these never throw
  DoesFileExist file -> PIO.doesFileExist file
  DoesDirExist dir -> PIO.doesDirExist dir
{-# INLINE readFSToIO #-}
