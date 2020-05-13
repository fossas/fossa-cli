module Effect.ReadFS
  ( -- * ReadFS Effect
    ReadFS (..),
    ReadFSErr (..),
    ReadFSIOC (..),

    -- * Reading raw file contents
    readContentsBS,
    readContentsText,

    -- * Resolving relative filepaths
    resolveFile,
    resolveFile',
    resolveDir,
    resolveDir',

    -- * Checking whether files exist
    doesFileExist,
    doesDirExist,

    -- * Parsing file contents
    readContentsParser,
    readContentsParser',
    readContentsJson,
    readContentsJson',
    readContentsYaml,
    readContentsYaml',
    readContentsXML,
    readContentsXML',
    module X,
  )
where

import Control.Algebra as X
import Control.Effect.Error
import Control.Carrier.Error.Either
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Yaml (decodeEither', prettyPrintParseException)
import Parse.XML (FromXML, parseXML, xmlErrorPretty)
import Prologue
import qualified Path.IO as PIO
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Error (errorBundlePretty)

data ReadFS m k where
  ReadContentsBS' :: Path x File -> ReadFS m (Either ReadFSErr ByteString)
  ReadContentsText' :: Path x File -> ReadFS m (Either ReadFSErr Text)
  DoesFileExist :: Path x File -> ReadFS m Bool
  DoesDirExist :: Path x Dir -> ReadFS m Bool
  ResolveFile' :: Path Abs Dir -> Text -> ReadFS m (Either ReadFSErr (Path Abs File))
  ResolveDir' :: Path Abs Dir -> Text -> ReadFS m (Either ReadFSErr (Path Abs Dir))

data ReadFSErr
  = -- | A file couldn't be read. file, err
    FileReadError FilePath Text
  | -- | A file's contents couldn't be parsed. TODO: ask user to help with this. file, err
    FileParseError FilePath Text
  | -- | An IOException was thrown when resolving a file/directory
    ResolveError FilePath FilePath Text
  deriving (Eq, Ord, Show, Generic, Typeable)

instance E.Exception ReadFSErr

-- | Read file contents into a strict 'ByteString'
readContentsBS' :: Has ReadFS sig m => Path b File -> m (Either ReadFSErr ByteString)
readContentsBS' path = send (ReadContentsBS' path)

-- | Read file contents into a strict 'ByteString'
readContentsBS :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path b File -> m ByteString
readContentsBS = fromEither <=< readContentsBS'

-- | Read file contents into a strict 'Text'
readContentsText' :: Has ReadFS sig m => Path b File -> m (Either ReadFSErr Text)
readContentsText' path = send (ReadContentsText' path)

-- | Read file contents into a strict 'Text'
readContentsText :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path b File -> m Text
readContentsText = fromEither <=< readContentsText'

-- | Resolve a relative filepath to a file
resolveFile' :: Has ReadFS sig m => Path Abs Dir -> Text -> m (Either ReadFSErr (Path Abs File))
resolveFile' base path = send (ResolveFile' base path)

-- | Resolve a relative filepath to a file
resolveFile :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Abs Dir -> Text -> m (Path Abs File)
resolveFile dir path = fromEither =<< resolveFile' dir path

-- | Resolve a relative filepath to a directory
resolveDir' :: Has ReadFS sig m => Path Abs Dir -> Text -> m (Either ReadFSErr (Path Abs Dir))
resolveDir' base path = send (ResolveDir' base path)

-- | Resolve a relative filepath to a directory
resolveDir :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Abs Dir -> Text -> m (Path Abs Dir)
resolveDir dir path = fromEither =<< resolveDir' dir path

-- | Check whether a file exists
doesFileExist :: Has ReadFS sig m => Path b File -> m Bool
doesFileExist path = send (DoesFileExist path)

-- | Check whether a directory exists
doesDirExist :: Has ReadFS sig m => Path b Dir -> m Bool
doesDirExist path = send (DoesDirExist path)

type Parser = Parsec Void Text

-- | Read from a file, parsing its contents
readContentsParser :: forall a sig m b. (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Parser a -> Path b File -> m a
readContentsParser parser file = do
  contents <- readContentsText file
  case runParser parser (toFilePath file) contents of
    Left err -> throwError (FileParseError (toFilePath file) (T.pack (errorBundlePretty err)))
    Right a -> pure a

-- | Read from a file, parsing its contents
readContentsParser' :: Has ReadFS sig m => Parser a -> Path b File -> m (Either ReadFSErr a)
readContentsParser' parser file = runError $ readContentsParser parser file

-- | Read JSON from a file
readContentsJson :: (FromJSON a, Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path b File -> m a
readContentsJson file = do
  contents <- readContentsBS file
  case eitherDecodeStrict contents of
    Left err -> throwError (FileParseError (toFilePath file) (T.pack err))
    Right a -> pure a

-- | Read JSON from a file
readContentsJson' :: (FromJSON a, Has ReadFS sig m) => Path b File -> m (Either ReadFSErr a)
readContentsJson' = runError . readContentsJson

-- | Read YAML from a file
readContentsYaml :: (FromJSON a, Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path b File -> m a
readContentsYaml file = do
  contents <- readContentsBS file
  case decodeEither' contents of
    Left err -> throwError (FileParseError (toFilePath file) (T.pack $ prettyPrintParseException err))
    Right a -> pure a

-- | Read YAML from a file
readContentsYaml' :: (FromJSON a, Has ReadFS sig m) => Path b File -> m (Either ReadFSErr a)
readContentsYaml' = runError . readContentsYaml

-- | Read XML from a file
readContentsXML :: (FromXML a, Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path b File -> m a
readContentsXML file = do
  contents <- readContentsText file
  case parseXML contents of
    Left err -> throwError (FileParseError (toFilePath file) (xmlErrorPretty err))
    Right a -> pure a

-- | Read XML from a file
readContentsXML' :: (FromXML a, Has ReadFS sig m) => Path b File -> m (Either ReadFSErr a)
readContentsXML' = runError . readContentsXML

newtype ReadFSIOC m a = ReadFSIOC {runReadFSIO :: m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

instance (Algebra sig m, MonadIO m) => Algebra (ReadFS :+: sig) (ReadFSIOC m) where
  alg hdl sig ctx = ReadFSIOC $ do
    case sig of
      L (ReadContentsBS' file) -> do
        res <- catchingIO (BS.readFile (toFilePath file)) (FileReadError (toFilePath file))
        pure (res <$ ctx)
      L (ReadContentsText' file) -> do
        res <- catchingIO (decodeUtf8 <$> BS.readFile (toFilePath file)) (FileReadError (toFilePath file))
        pure (res <$ ctx)
      L (ResolveFile' dir path) -> do
        res <- catchingIO (PIO.resolveFile dir (T.unpack path)) (ResolveError (toFilePath dir) (T.unpack path))
        pure (res <$ ctx)
      L (ResolveDir' dir path) -> do
        res <- catchingIO (PIO.resolveDir dir (T.unpack path)) (ResolveError (toFilePath dir) (T.unpack path))
        pure (res <$ ctx)
      -- NB: these never throw
      L (DoesFileExist file) -> (<$ ctx) <$> PIO.doesFileExist file
      L (DoesDirExist dir) -> (<$ ctx) <$> PIO.doesDirExist dir
      R other -> alg (runReadFSIO . hdl) other ctx
    where
      catchingIO :: IO a -> (Text -> ReadFSErr) -> m (Either ReadFSErr a)
      catchingIO io mangle = liftIO $ E.catch (Right <$> io) (\(e :: E.IOException) -> pure (Left (mangle (T.pack (show e)))))
