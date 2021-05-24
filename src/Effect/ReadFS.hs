{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

    -- * Listing a directory
    listDir,
    listDir',

    -- * Parsing file contents
    readContentsParser,
    readContentsJson,
    readContentsToml,
    readContentsYaml,
    readContentsXML,
    module X,
  )
where

import Control.Algebra as X
import Control.Applicative (Alternative)
import Control.Effect.Diagnostics
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Record
import Control.Effect.Record.TH (deriveRecordable)
import Control.Effect.Replay
import Control.Effect.Replay.TH (deriveReplayable)
import qualified Control.Exception as E
import Control.Monad ((<=<))
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Kind (Type)
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc (pretty, line, indent, vsep)
import Data.Void (Void)
import Data.Yaml (decodeEither', prettyPrintParseException)
import GHC.Generics (Generic)
import Parse.XML (FromXML, parseXML, xmlErrorPretty)
import Path
import qualified Path.IO as PIO
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Toml

data ReadFS (m :: Type -> Type) k where
  ReadContentsBS' :: Path x File -> ReadFS m (Either ReadFSErr ByteString)
  ReadContentsText' :: Path x File -> ReadFS m (Either ReadFSErr Text)
  DoesFileExist :: Path x File -> ReadFS m Bool
  DoesDirExist :: Path x Dir -> ReadFS m Bool
  ResolveFile' :: Path Abs Dir -> Text -> ReadFS m (Either ReadFSErr (Path Abs File))
  ResolveDir' :: Path Abs Dir -> Text -> ReadFS m (Either ReadFSErr (Path Abs Dir))
  ListDir :: Path Abs Dir -> ReadFS m (Either ReadFSErr ([Path Abs Dir], [Path Abs File]))

data ReadFSErr
  = -- | A file couldn't be read. file, err
    FileReadError FilePath Text
  | -- | A file's contents couldn't be parsed. TODO: ask user to help with this. file, err
    FileParseError FilePath Text
  | -- | An IOException was thrown when resolving a file/directory
    ResolveError FilePath FilePath Text
  | -- | An IOException was thrown when listing a directory
    ListDirError FilePath Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ReadFSErr
instance RecordableValue ReadFSErr
instance FromJSON ReadFSErr
instance ReplayableValue ReadFSErr
$(deriveRecordable ''ReadFS)
$(deriveReplayable ''ReadFS)

instance ToDiagnostic ReadFSErr where
  renderDiagnostic = \case
    FileReadError path err -> "Error reading file " <> pretty path <> ":" <> line <> indent 4 (pretty err)
    FileParseError path err -> "Error parsing file " <> pretty path <> ":" <> line <> indent 4 (pretty err)
    ResolveError base rel err ->
      "Error resolving a relative file:" <> line
        <> indent
          4
          ( vsep
              [ "base: " <> pretty base
              , "relative: " <> pretty rel
              , "error: " <> pretty err
              ]
          )
    ListDirError dir err -> "Error listing directory contents at " <> pretty dir <> ":" <> line <> indent 2 (pretty err)

-- | Read file contents into a strict 'ByteString'
readContentsBS' :: Has ReadFS sig m => Path b File -> m (Either ReadFSErr ByteString)
readContentsBS' path = send (ReadContentsBS' path)

-- | Read file contents into a strict 'ByteString'
readContentsBS :: (Has ReadFS sig m, Has Diagnostics sig m) => Path b File -> m ByteString
readContentsBS = fromEither <=< readContentsBS'

-- | Read file contents into a strict 'Text'
readContentsText' :: Has ReadFS sig m => Path b File -> m (Either ReadFSErr Text)
readContentsText' path = send (ReadContentsText' path)

-- | Read file contents into a strict 'Text'
readContentsText :: (Has ReadFS sig m, Has Diagnostics sig m) => Path b File -> m Text
readContentsText = fromEither <=< readContentsText'

-- | Resolve a relative filepath to a file
resolveFile' :: Has ReadFS sig m => Path Abs Dir -> Text -> m (Either ReadFSErr (Path Abs File))
resolveFile' base path = send (ResolveFile' base path)

-- | Resolve a relative filepath to a file
resolveFile :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Text -> m (Path Abs File)
resolveFile dir path = fromEither =<< resolveFile' dir path

-- | Resolve a relative filepath to a directory
resolveDir' :: Has ReadFS sig m => Path Abs Dir -> Text -> m (Either ReadFSErr (Path Abs Dir))
resolveDir' base path = send (ResolveDir' base path)

-- | Resolve a relative filepath to a directory
resolveDir :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Text -> m (Path Abs Dir)
resolveDir dir path = fromEither =<< resolveDir' dir path

-- | Check whether a file exists
doesFileExist :: Has ReadFS sig m => Path b File -> m Bool
doesFileExist path = send (DoesFileExist path)

-- | Check whether a directory exists
doesDirExist :: Has ReadFS sig m => Path b Dir -> m Bool
doesDirExist path = send (DoesDirExist path)

listDir' :: Has ReadFS sig m => Path Abs Dir -> m (Either ReadFSErr ([Path Abs Dir], [Path Abs File]))
listDir' = send . ListDir

listDir :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m ([Path Abs Dir], [Path Abs File])
listDir dir = fromEither =<< listDir' dir

type Parser = Parsec Void Text

-- | Read from a file, parsing its contents
readContentsParser :: forall a sig m b. (Has ReadFS sig m, Has Diagnostics sig m) => Parser a -> Path b File -> m a
readContentsParser parser file = context ("Parsing file '" <> T.pack (toFilePath file) <> "'") $ do
  contents <- readContentsText file
  case runParser parser (toFilePath file) contents of
    Left err -> fatal (FileParseError (toFilePath file) (T.pack (errorBundlePretty err)))
    Right a -> pure a

-- | Read JSON from a file
readContentsJson :: (FromJSON a, Has ReadFS sig m, Has Diagnostics sig m) => Path b File -> m a
readContentsJson file = context ("Parsing JSON file '" <> T.pack (toFilePath file) <> "'") $ do
  contents <- readContentsBS file
  case eitherDecodeStrict contents of
    Left err -> fatal (FileParseError (toFilePath file) (T.pack err))
    Right a -> pure a

readContentsToml :: (Has ReadFS sig m, Has Diagnostics sig m) => Toml.TomlCodec a -> Path b File -> m a
readContentsToml codec file = context ("Parsing TOML file '" <> T.pack (toFilePath file) <> "'") $ do
  contents <- readContentsText file
  case Toml.decode codec contents of
    Left err -> fatal (FileParseError (toFilePath file) (Toml.prettyTomlDecodeErrors err))
    Right a -> pure a

-- | Read YAML from a file
readContentsYaml :: (FromJSON a, Has ReadFS sig m, Has Diagnostics sig m) => Path b File -> m a
readContentsYaml file = context ("Parsing YAML file '" <> T.pack (toFilePath file) <> "'") $ do
  contents <- readContentsBS file
  case decodeEither' contents of
    Left err -> fatal (FileParseError (toFilePath file) (T.pack $ prettyPrintParseException err))
    Right a -> pure a

-- | Read XML from a file
readContentsXML :: (FromXML a, Has ReadFS sig m, Has Diagnostics sig m) => Path b File -> m a
readContentsXML file = context ("Parsing XML file '" <> T.pack (toFilePath file) <> "'") $ do
  contents <- readContentsText file
  case parseXML contents of
    Left err -> fatal (FileParseError (toFilePath file) (xmlErrorPretty err))
    Right a -> pure a

newtype ReadFSIOC m a = ReadFSIOC {runReadFSIO :: m a}
  deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadFail)

instance Has (Lift IO) sig m => Algebra (ReadFS :+: sig) (ReadFSIOC m) where
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
      L (ListDir dir) -> do
        res <- catchingIO (PIO.listDir dir) (ListDirError (toFilePath dir))
        pure (res <$ ctx)
      -- NB: these never throw
      L (DoesFileExist file) -> (<$ ctx) <$> sendIO (PIO.doesFileExist file)
      L (DoesDirExist dir) -> (<$ ctx) <$> sendIO (PIO.doesDirExist dir)
      R other -> alg (runReadFSIO . hdl) other ctx
    where
      catchingIO :: IO a -> (Text -> ReadFSErr) -> m (Either ReadFSErr a)
      catchingIO io mangle = sendIO $ E.catch (Right <$> io) (\(e :: E.IOException) -> pure (Left (mangle (T.pack (show e)))))
