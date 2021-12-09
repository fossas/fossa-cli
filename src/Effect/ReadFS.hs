{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Effect.ReadFS (
  -- * ReadFS Effect
  ReadFS,
  ReadFSF (..),
  ReadFSErr (..),
  ReadFSIOC,
  runReadFSIO,

  -- * Reading raw file contents
  readContentsBS,
  readContentsBSLimit,
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

  -- * File identity information
  contentIsBinary,
  module X,
) where

import Control.Algebra as X
import Control.Carrier.Simple
import Control.Effect.Diagnostics
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Record
import Control.Effect.Record.TH (deriveRecordable)
import Control.Effect.Replay
import Control.Effect.Replay.TH (deriveReplayable)
import Control.Exception qualified as E
import Control.Monad ((<=<))
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.String.Conversion (decodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Void (Void)
import Data.Yaml (decodeEither', prettyPrintParseException)
import GHC.Generics (Generic)
import Parse.XML (FromXML, parseXML, xmlErrorPretty)
import Path
import Path.IO qualified as PIO
import Prettyprinter (indent, line, pretty, vsep)
import System.Directory qualified as Directory
import System.IO (IOMode (ReadMode), withFile)
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import Toml qualified

data ReadFSF a where
  ReadContentsBS' :: SomeBase File -> ReadFSF (Either ReadFSErr ByteString)
  ReadContentsBSLimit' :: SomeBase File -> Int -> ReadFSF (Either ReadFSErr ByteString)
  ReadContentsText' :: SomeBase File -> ReadFSF (Either ReadFSErr Text)
  DoesFileExist :: SomeBase File -> ReadFSF Bool
  DoesDirExist :: SomeBase Dir -> ReadFSF Bool
  ResolveFile' :: Path Abs Dir -> Text -> ReadFSF (Either ReadFSErr (Path Abs File))
  ResolveDir' :: Path Abs Dir -> Text -> ReadFSF (Either ReadFSErr (Path Abs Dir))
  ListDir :: Path Abs Dir -> ReadFSF (Either ReadFSErr ([Path Abs Dir], [Path Abs File]))

type ReadFS = Simple ReadFSF

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
$(deriveRecordable ''ReadFSF)
$(deriveReplayable ''ReadFSF)

deriving instance Show (ReadFSF a)
deriving instance Eq (ReadFSF a)
deriving instance Ord (ReadFSF a)

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
readContentsBS' :: Has ReadFS sig m => Path Abs File -> m (Either ReadFSErr ByteString)
readContentsBS' path = sendSimple (ReadContentsBS' (Abs path))

-- | Read at most n bytes of file content into a strict 'ByteString'
readContentsBSLimit :: Has ReadFS sig m => Path Abs File -> Int -> m (Either ReadFSErr ByteString)
readContentsBSLimit path limit = sendSimple (ReadContentsBSLimit' (Abs path) limit)

-- | Read file contents into a strict 'ByteString'
readContentsBS :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m ByteString
readContentsBS = fromEither <=< readContentsBS'

-- | Read file contents into a strict 'Text'
readContentsText' :: Has ReadFS sig m => Path Abs File -> m (Either ReadFSErr Text)
readContentsText' path = sendSimple (ReadContentsText' (Abs path))

-- | Read file contents into a strict 'Text'
readContentsText :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m Text
readContentsText = fromEither <=< readContentsText'

-- | Resolve a relative filepath to a file
resolveFile' :: Has ReadFS sig m => Path Abs Dir -> Text -> m (Either ReadFSErr (Path Abs File))
resolveFile' base path = sendSimple (ResolveFile' base path)

-- | Resolve a relative filepath to a file
resolveFile :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Text -> m (Path Abs File)
resolveFile dir path = fromEither =<< resolveFile' dir path

-- | Resolve a relative filepath to a directory
resolveDir' :: Has ReadFS sig m => Path Abs Dir -> Text -> m (Either ReadFSErr (Path Abs Dir))
resolveDir' base path = sendSimple (ResolveDir' base path)

-- | Resolve a relative filepath to a directory
resolveDir :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Text -> m (Path Abs Dir)
resolveDir dir path = fromEither =<< resolveDir' dir path

-- | Check whether a file exists
doesFileExist :: Has ReadFS sig m => Path Abs File -> m Bool
doesFileExist path = sendSimple (DoesFileExist (Abs path))

-- | Check whether a directory exists
doesDirExist :: Has ReadFS sig m => Path Abs Dir -> m Bool
doesDirExist path = sendSimple (DoesDirExist (Abs path))

listDir' :: Has ReadFS sig m => Path Abs Dir -> m (Either ReadFSErr ([Path Abs Dir], [Path Abs File]))
listDir' = sendSimple . ListDir

listDir :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m ([Path Abs Dir], [Path Abs File])
listDir dir = fromEither =<< listDir' dir

-- | Determine if a file is binary using the same method as git:
-- "is there a zero byte in the first 8000 bytes of the file"
contentIsBinary :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m Bool
contentIsBinary file = do
  attemptedContent <- readContentsBSLimit file 8000
  content <- fromEither attemptedContent
  pure $ BS.elem 0 content

type Parser = Parsec Void Text

-- | Read from a file, parsing its contents
readContentsParser :: forall a sig m. (Has ReadFS sig m, Has Diagnostics sig m) => Parser a -> Path Abs File -> m a
readContentsParser parser file = context ("Parsing file '" <> toText (fromAbsFile file) <> "'") $ do
  contents <- readContentsText file
  case runParser parser (fromAbsFile file) contents of
    Left err -> fatal (FileParseError (fromAbsFile file) (toText (errorBundlePretty err)))
    Right a -> pure a

-- | Read JSON from a file
readContentsJson :: (FromJSON a, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m a
readContentsJson file = context ("Parsing JSON file '" <> toText (fromAbsFile file) <> "'") $ do
  contents <- readContentsBS file
  case eitherDecodeStrict contents of
    Left err -> fatal (FileParseError (fromAbsFile file) (toText err))
    Right a -> pure a

readContentsToml :: (Has ReadFS sig m, Has Diagnostics sig m) => Toml.TomlCodec a -> Path Abs File -> m a
readContentsToml codec file = context ("Parsing TOML file '" <> toText (fromAbsFile file) <> "'") $ do
  contents <- readContentsText file
  case Toml.decode codec contents of
    Left err -> fatal (FileParseError (fromAbsFile file) (Toml.prettyTomlDecodeErrors err))
    Right a -> pure a

-- | Read YAML from a file
readContentsYaml :: (FromJSON a, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m a
readContentsYaml file = context ("Parsing YAML file '" <> toText (fromAbsFile file) <> "'") $ do
  contents <- readContentsBS file
  case decodeEither' contents of
    Left err -> fatal (FileParseError (fromAbsFile file) (toText $ prettyPrintParseException err))
    Right a -> pure a

-- | Read XML from a file
readContentsXML :: (FromXML a, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m a
readContentsXML file = context ("Parsing XML file '" <> toText (fromAbsFile file) <> "'") $ do
  contents <- readContentsText file
  case parseXML contents of
    Left err -> fatal (FileParseError (fromAbsFile file) (xmlErrorPretty err))
    Right a -> pure a

type ReadFSIOC = SimpleC ReadFSF

runReadFSIO :: Has (Lift IO) sig m => ReadFSIOC m a -> m a
runReadFSIO = interpret $ \case
  ReadContentsBS' file -> do
    BS.readFile (fromSomeFile file)
      `catchingIO` FileReadError (fromSomeFile file)
  ReadContentsBSLimit' file limit -> do
    readContentsBSLimit' file limit
      `catchingIO` FileReadError (fromSomeFile file)
  ReadContentsText' file -> do
    (decodeUtf8 <$> BS.readFile (fromSomeFile file))
      `catchingIO` FileReadError (fromSomeFile file)
  ResolveFile' dir path -> do
    PIO.resolveFile dir (toString path)
      `catchingIO` ResolveError (toFilePath dir) (toString path)
  ResolveDir' dir path -> do
    PIO.resolveDir dir (toString path)
      `catchingIO` ResolveError (toFilePath dir) (toString path)
  ListDir dir -> do
    PIO.listDir dir
      `catchingIO` ListDirError (toFilePath dir)
  -- NB: these never throw
  DoesFileExist file -> sendIO (Directory.doesFileExist (fromSomeFile file))
  DoesDirExist dir -> sendIO (Directory.doesDirectoryExist (fromSomeDir dir))

readContentsBSLimit' :: SomeBase File -> Int -> IO ByteString
readContentsBSLimit' file limit = withFile (fromSomeFile file) ReadMode $ \handle -> BS.hGetSome handle limit

catchingIO :: Has (Lift IO) sig m => IO a -> (Text -> ReadFSErr) -> m (Either ReadFSErr a)
catchingIO io mangle = sendIO $ E.catch (Right <$> io) (\(e :: E.IOException) -> pure (Left (mangle (toText (show e)))))
