module Effect.ReadFS
  ( -- * ReadFS Effect
    ReadFS(..)
  , ReadFSErr(..)
  , ReadFSIOC(..)
  , runReadFSIO

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

  , module X
  ) where

import Prologue

import Control.Algebra as X
import Control.Effect.Error
import Control.Carrier.Error.Either
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Yaml (decodeEither', prettyPrintParseException)
import Parse.XML (FromXML, parseXML, xmlErrorPretty)
import qualified Path.IO as PIO
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Error (errorBundlePretty)

data ReadFS m k
  = forall x. ReadContentsBS' (Path x File) (Either ReadFSErr ByteString -> m k)
  | forall x. ReadContentsText' (Path x File) (Either ReadFSErr Text -> m k)
  | forall x. DoesFileExist (Path x File) (Bool -> m k)
  | forall x. DoesDirExist (Path x Dir) (Bool -> m k)
  | ResolveFile' (Path Abs Dir) Text (Either ReadFSErr (Path Abs File) -> m k)
  | ResolveDir' (Path Abs Dir) Text (Either ReadFSErr (Path Abs Dir) -> m k)

data ReadFSErr =
    FileReadError FilePath Text -- ^ A file couldn't be read. file, err
  | FileParseError FilePath Text -- ^ A file's contents couldn't be parsed. TODO: ask user to help with this. file, err
  | ResolveError FilePath FilePath Text -- ^ An IOException was thrown when resolving a file/directory
  deriving (Eq, Ord, Show, Generic, Typeable)

instance E.Exception ReadFSErr

instance HFunctor ReadFS where
  hmap f = \case
    ReadContentsBS' path k -> ReadContentsBS' path (f . k)
    ReadContentsText' path k -> ReadContentsText' path (f . k)
    DoesFileExist path k -> DoesFileExist path (f . k)
    DoesDirExist path k -> DoesDirExist path (f . k)
    ResolveFile' base path k -> ResolveFile' base path (f . k)
    ResolveDir' base path k -> ResolveDir' base path (f . k)

instance Effect ReadFS where
  thread ctx handle = \case
    ReadContentsBS' path k -> ReadContentsBS' path (handle . (<$ ctx) . k)
    ReadContentsText' path k -> ReadContentsText' path (handle . (<$ ctx) . k)
    DoesFileExist path k -> DoesFileExist path (handle . (<$ ctx) . k)
    DoesDirExist path k -> DoesDirExist path (handle . (<$ ctx) . k)
    ResolveFile' base path k -> ResolveFile' base path (handle . (<$ ctx) . k)
    ResolveDir' base path k -> ResolveDir' base path (handle . (<$ ctx) . k)

-- | Read file contents into a strict 'ByteString'
readContentsBS' :: Has ReadFS sig m => Path b File -> m (Either ReadFSErr ByteString)
readContentsBS' path = send (ReadContentsBS' path pure)

-- | Read file contents into a strict 'ByteString'
readContentsBS :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path b File -> m ByteString
readContentsBS = fromEither <=< readContentsBS'

-- | Read file contents into a strict 'Text'
readContentsText' :: Has ReadFS sig m => Path b File -> m (Either ReadFSErr Text)
readContentsText' path = send (ReadContentsText' path pure)

-- | Read file contents into a strict 'Text'
readContentsText :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path b File -> m Text
readContentsText = fromEither <=< readContentsText'

-- | Resolve a relative filepath to a file
resolveFile' :: Has ReadFS sig m => Path Abs Dir -> Text -> m (Either ReadFSErr (Path Abs File))
resolveFile' base path = send (ResolveFile' base path pure)

-- | Resolve a relative filepath to a file
resolveFile :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Abs Dir -> Text -> m (Path Abs File)
resolveFile dir path = fromEither =<< resolveFile' dir path

-- | Resolve a relative filepath to a directory
resolveDir' :: Has ReadFS sig m => Path Abs Dir -> Text -> m (Either ReadFSErr (Path Abs Dir))
resolveDir' base path = send (ResolveDir' base path pure)

-- | Resolve a relative filepath to a directory
resolveDir :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Abs Dir -> Text -> m (Path Abs Dir)
resolveDir dir path = fromEither =<< resolveDir' dir path

-- | Check whether a file exists
doesFileExist :: Has ReadFS sig m => Path b File -> m Bool
doesFileExist path = send (DoesFileExist path pure)

-- | Check whether a directory exists
doesDirExist :: Has ReadFS sig m => Path b Dir -> m Bool
doesDirExist path = send (DoesDirExist path pure)

type Parser = Parsec Void Text

-- | Read from a file, parsing its contents
readContentsParser :: forall a sig m b. (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Parser a -> Path b File -> m a
readContentsParser parser file = do
  contents <- readContentsText file
  case runParser parser (toFilePath file) contents of
    Left err -> throwError (FileParseError (toFilePath file) (T.pack (errorBundlePretty err)))
    Right a -> pure a

-- | Read from a file, parsing its contents
readContentsParser' :: forall a sig m b. (Has ReadFS sig m, Effect sig) => Parser a -> Path b File -> m (Either ReadFSErr a)
readContentsParser' parser file = runError $ readContentsParser parser file

-- | Read JSON from a file
readContentsJson :: (FromJSON a, Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path b File -> m a
readContentsJson file = do
  contents <- readContentsBS file
  case eitherDecodeStrict contents of
    Left err -> throwError (FileParseError (toFilePath file) (T.pack err))
    Right a -> pure a

-- | Read JSON from a file
readContentsJson' :: (FromJSON a, Has ReadFS sig m, Effect sig) => Path b File -> m (Either ReadFSErr a)
readContentsJson' = runError . readContentsJson

-- | Read YAML from a file
readContentsYaml :: (FromJSON a, Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path b File -> m a
readContentsYaml file = do
  contents <- readContentsBS file
  case decodeEither' contents of
    Left err -> throwError (FileParseError (toFilePath file) (T.pack$ prettyPrintParseException err))
    Right a -> pure a

-- | Read YAML from a file
readContentsYaml' :: (FromJSON a, Has ReadFS sig m, Effect sig) => Path b File -> m (Either ReadFSErr a)
readContentsYaml' = runError . readContentsYaml

-- | Read XML from a file
readContentsXML :: (FromXML a, Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path b File -> m a
readContentsXML file = do
  contents <- readContentsText file
  case parseXML contents of
    Left err -> throwError (FileParseError (toFilePath file) (xmlErrorPretty err))
    Right a -> pure a

-- | Read XML from a file
readContentsXML' :: (FromXML a, Has ReadFS sig m, Effect sig) => Path b File -> m (Either ReadFSErr a)
readContentsXML' = runError . readContentsXML

runReadFSIO :: ReadFSIOC m a -> m a
runReadFSIO = coerce

newtype ReadFSIOC m a = ReadFSIOC { runReadFSIOC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

instance (Algebra sig m, MonadIO m) => Algebra (ReadFS :+: sig) (ReadFSIOC m) where
  alg (L act) = case act of
    ReadContentsBS' file k -> (k =<<) . ReadFSIOC $ liftIO $
      (Right <$> BS.readFile (toFilePath file))
      `E.catch`
      (\(e :: E.IOException) -> pure (Left (FileReadError (toFilePath file) (T.pack (show e)))))
    ReadContentsText' file k -> (k =<<) . ReadFSIOC $ liftIO $
      (Right . decodeUtf8 <$> BS.readFile (toFilePath file))
      `E.catch`
      (\(e :: E.IOException) -> pure (Left (FileReadError (toFilePath file) (T.pack (show e)))))
    ResolveFile' dir path k -> (k =<<) . ReadFSIOC $ liftIO $
      (Right <$> PIO.resolveFile dir (T.unpack path))
      `E.catch`
      (\(e :: E.IOException) -> pure (Left (ResolveError (toFilePath dir) (T.unpack path) (T.pack (show e)))))
    ResolveDir' dir path k -> (k =<<) . ReadFSIOC $ liftIO $
      (Right <$> PIO.resolveDir dir (T.unpack path))
      `E.catch`
      (\(e :: E.IOException) -> pure (Left (ResolveError (toFilePath dir) (T.unpack path) (T.pack (show e)))))
    -- NB: these never throw
    DoesFileExist file k -> k =<< PIO.doesFileExist file
    DoesDirExist dir k -> k =<< PIO.doesDirExist dir
  alg (R other) = ReadFSIOC (alg (handleCoercible other))
