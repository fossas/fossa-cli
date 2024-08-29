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

  -- * Get current directory
  getCurrentDir,
  getCurrentDir',

  -- * Resolving relative filepaths
  resolveFile,
  resolveFile',
  resolveDir,
  resolveDir',

  -- * Resolving existing paths
  resolvePath,
  resolvePath',

  -- * Checking whether files exist
  doesFileExist,
  doesDirExist,

  -- * Listing a directory
  listDir,
  listDir',

  -- * Parsing file contents
  readContentsParser,
  readContentsParserBS,
  readContentsJson,
  readContentsToml,
  readContentsYaml,
  readContentsXML,

  -- * Reading file contents, redacting the content from logging and diagnostics.
  readRedactedContentsBS,
  readRedactedContentsBSLimit,
  readRedactedContentsText,

  -- * File identity information
  contentIsBinary,
  DirID (..),
  getIdentifier,
  getIdentifier',

  -- * misc
  catchingIO,
  module X,
) where

import App.Support (supportUrl)
import Control.Algebra as X
import Control.Carrier.Simple (
  Simple,
  SimpleC,
  interpret,
  sendSimple,
 )
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic (..),
  context,
  errSupport,
  fatal,
  fromEither,
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Record (RecordableValue, Redacted (..))
import Control.Effect.Record.TH (deriveRecordable)
import Control.Effect.Replay (ReplayableValue)
import Control.Exception qualified as E
import Control.Exception.Extra (safeCatch)
import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either.Combinators (mapRight)
import Data.String.Conversion (decodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Text.Extra (showT)
import Data.Void (Void)
import Data.Yaml (decodeEither', prettyPrintParseException)
import Effect.Logger (renderIt)
import Errata (Errata (..))
import GHC.Generics (Generic)
import Parse.XML (FromXML, parseXML, xmlErrorPretty)
import Path (
  Abs,
  Dir,
  File,
  Path,
  SomeBase (Abs),
  fromSomeDir,
  fromSomeFile,
  parseSomeDir,
  parseSomeFile,
 )
import Path.Extra (SomePath (..))
import Path.IO qualified as PIO
import Prettyprinter (indent, pretty, vsep)
import System.Directory qualified as Directory
import System.FilePath qualified as FP
import System.IO (IOMode (ReadMode), withFile)
import System.PosixCompat (isRegularFile)
import System.PosixCompat.Files (isDirectory)
import System.PosixCompat.Files qualified as Posix
import System.PosixCompat.Types (CDev (..), CIno (..))
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import Toml qualified
import Toml.Schema qualified

-- | A unique file identifier for a directory.
-- Uniqueness is guaranteed within a single OS.
data DirID = DirID {dirFileID :: Integer, dirDeviceID :: Integer} deriving (Show, Eq, Ord, Generic)

instance ToJSON DirID
instance RecordableValue DirID
instance FromJSON DirID
instance ReplayableValue DirID

data ReadFSF a where
  GetCurrentDir :: ReadFSF (Either ReadFSErr (Path Abs Dir))
  ReadContentsBS' :: SomeBase File -> ReadFSF (Either ReadFSErr ByteString)
  ReadContentsBSLimit' :: SomeBase File -> Int -> ReadFSF (Either ReadFSErr ByteString)
  ReadContentsText' :: SomeBase File -> ReadFSF (Either ReadFSErr Text)
  ReadRedactedContentsBS' :: SomeBase File -> ReadFSF (Either ReadFSErr (Redacted ByteString))
  ReadRedactedContentsBSLimit' :: SomeBase File -> Int -> ReadFSF (Either ReadFSErr (Redacted ByteString))
  ReadRedactedContentsText' :: SomeBase File -> ReadFSF (Either ReadFSErr (Redacted Text))
  DoesFileExist :: SomeBase File -> ReadFSF Bool
  DoesDirExist :: SomeBase Dir -> ReadFSF Bool
  ResolveFile' :: Path Abs Dir -> Text -> ReadFSF (Either ReadFSErr (Path Abs File))
  ResolveDir' :: Path Abs Dir -> Text -> ReadFSF (Either ReadFSErr (Path Abs Dir))
  ResolvePath :: Path Abs Dir -> FilePath -> ReadFSF (Either ReadFSErr SomePath)
  ListDir :: Path Abs Dir -> ReadFSF (Either ReadFSErr ([Path Abs Dir], [Path Abs File]))
  GetIdentifier :: Path Abs Dir -> ReadFSF (Either ReadFSErr DirID)

type ReadFS = Simple ReadFSF

data ReadFSErr
  = -- | A file couldn't be read. file, err
    FileReadError FilePath Text
  | -- | A file's contents couldn't be parsed.
    FileParseError FilePath Text
  | -- | An IOException was thrown when resolving a file/directory
    ResolveError FilePath FilePath Text
  | -- | An IOException was thrown when listing a directory
    ListDirError FilePath Text
  | -- | A file had an unknown type (i.e. not a regular file or directory)
    NotDirOrFile FilePath
  | -- | A file was found to be BOTH a directory and regular file, and we cannot decide which it really is.
    UndeterminableFileType FilePath
  | -- | An IOException was thrown when resolving the current directory
    CurrentDirError Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ReadFSErr
instance RecordableValue ReadFSErr
instance FromJSON ReadFSErr
instance ReplayableValue ReadFSErr
$(deriveRecordable ''ReadFSF)

deriving instance Show (ReadFSF a)
deriving instance Eq (ReadFSF a)
deriving instance Ord (ReadFSF a)

instance ToDiagnostic ReadFSErr where
  renderDiagnostic = \case
    FileReadError path err -> do
      let header = "reading file: " <> toText path
          body = renderIt $ vsep [indent 2 (pretty err)]
      Errata (Just header) [] (Just body)
    FileParseError path err -> do
      let header = "parsing file: " <> toText path
          body = renderIt $ vsep [indent 2 (pretty err)]
      Errata (Just header) [] (Just body)
    ResolveError base rel err -> do
      let header = "resolving a relative file"
          body =
            renderIt $
              indent
                2
                ( vsep
                    [ "base: " <> pretty base
                    , "relative: " <> pretty rel
                    , "error: " <> pretty err
                    ]
                )
      Errata (Just header) [] (Just body)
    ListDirError dir err -> do
      let header = "listing directory contents at: " <> toText dir
          body = renderIt $ vsep [indent 2 (pretty err)]
      Errata (Just header) [] (Just body)
    NotDirOrFile path -> do
      let header = "Path was not a dir or file, unknown type: " <> toText path
      Errata (Just header) [] Nothing
    UndeterminableFileType path -> do
      let header = "Path is both a file and a directory, which should be impossible: " <> toText path
          body =
            renderIt $
              vsep
                [ "Please report this as a bug to " <> pretty supportUrl <> ", and include the following info if possible:"
                , indent 2 $
                    vsep
                      [ "- Operating system"
                      , "- File system"
                      , "- Output of 'fossa --version'"
                      ]
                ]
      Errata (Just header) [] (Just body)
    CurrentDirError err -> do
      let header = "resolving the current directory "
          body = renderIt $ vsep [indent 2 (pretty err)]
      Errata (Just header) [] (Just body)

fileParseErrorSupportMsg :: Path Abs File -> Text
fileParseErrorSupportMsg file = "If you believe this to be a defect, please report a bug to FOSSA support at " <> supportUrl <> ", with a copy of: " <> toText file

-- | Read file contents into a strict 'ByteString'
readContentsBS' :: Has ReadFS sig m => Path Abs File -> m (Either ReadFSErr ByteString)
readContentsBS' path = sendSimple (ReadContentsBS' (Abs path))

-- | Read file contents into a strict 'ByteString'
readContentsBS :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m ByteString
readContentsBS = fromEither <=< readContentsBS'

-- | Read at most n bytes of file content into a strict 'ByteString'
readContentsBSLimit' :: Has ReadFS sig m => Path Abs File -> Int -> m (Either ReadFSErr ByteString)
readContentsBSLimit' path limit = sendSimple (ReadContentsBSLimit' (Abs path) limit)

-- | Read at most n bytes of file content into a strict 'ByteString'
readContentsBSLimit :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> Int -> m ByteString
readContentsBSLimit path limit = readContentsBSLimit' path limit >>= fromEither

-- | Read file contents into a strict 'Text'
readContentsText' :: Has ReadFS sig m => Path Abs File -> m (Either ReadFSErr Text)
readContentsText' path = sendSimple (ReadContentsText' (Abs path))

-- | Read file contents into a strict 'Text'
readContentsText :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m Text
readContentsText = fromEither <=< readContentsText'

-- | Read file contents into a strict 'ByteString', redacting the content from debug and log output
readRedactedContentsBS' :: Has ReadFS sig m => Path Abs File -> m (Either ReadFSErr (Redacted ByteString))
readRedactedContentsBS' path = sendSimple (ReadRedactedContentsBS' (Abs path))

-- | Read file contents into a strict 'ByteString', redacting the content from debug and log output
readRedactedContentsBS :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Redacted ByteString)
readRedactedContentsBS = fromEither <=< readRedactedContentsBS'

-- | Read at most n bytes of file content into a strict 'ByteString', redacting the content from debug and log output
readRedactedContentsBSLimit' :: Has ReadFS sig m => Path Abs File -> Int -> m (Either ReadFSErr (Redacted ByteString))
readRedactedContentsBSLimit' path limit = sendSimple (ReadRedactedContentsBSLimit' (Abs path) limit)

readRedactedContentsBSLimit :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> Int -> m (Redacted ByteString)
readRedactedContentsBSLimit path limit = readRedactedContentsBSLimit' path limit >>= fromEither

-- | Read file contents into a strict 'Text', redacting the content from debug and log output
readRedactedContentsText' :: Has ReadFS sig m => Path Abs File -> m (Either ReadFSErr (Redacted Text))
readRedactedContentsText' path = sendSimple (ReadRedactedContentsText' (Abs path))

-- | Read file contents into a strict 'Text', redacting the content from debug and log output
readRedactedContentsText :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Redacted Text)
readRedactedContentsText = fromEither <=< readRedactedContentsText'

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

-- | Resolve some path to an existing
resolvePath :: Has ReadFS sig m => Path Abs Dir -> FilePath -> m (Either ReadFSErr SomePath)
resolvePath root = sendSimple . ResolvePath root

resolvePath' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> FilePath -> m SomePath
resolvePath' root = fromEither <=< resolvePath root

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

-- | Get a unique identifier for a directory.
-- This follows symlinks and the identifier will be the same regardless of the path.
getIdentifier' :: Has ReadFS sig m => Path Abs Dir -> m (Either ReadFSErr DirID)
getIdentifier' = sendSimple . GetIdentifier

-- | Get a unique identifier for a directory.
-- This follows symlinks and the identifier will be the same regardless of the path.
getIdentifier :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m DirID
getIdentifier path = fromEither =<< getIdentifier' path

-- | Determine if a file is binary using the same method as git:
-- "is there a zero byte in the first 8000 bytes of the file"
contentIsBinary :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m Bool
contentIsBinary file = do
  attemptedContent <- readRedactedContentsBSLimit' file 8000
  content <- fromEither attemptedContent
  pure . BS.elem 0 $ unRedact content

-- | Get the current directory of the process.
getCurrentDir' :: (Has ReadFS sig m) => m (Either ReadFSErr (Path Abs Dir))
getCurrentDir' = sendSimple GetCurrentDir

-- | Get the current directory of the process.
getCurrentDir :: (Has ReadFS sig m, Has Diagnostics sig m) => m (Path Abs Dir)
getCurrentDir = getCurrentDir' >>= fromEither

type Parser = Parsec Void Text

-- | Read from a file, parsing its contents
readContentsParser :: forall a sig m. (Has ReadFS sig m, Has Diagnostics sig m) => Parser a -> Path Abs File -> m a
readContentsParser parser file = context ("Parsing file '" <> toText (toString file) <> "'") $ do
  contents <- readContentsText file
  case runParser parser (toString file) contents of
    Left err -> errSupport (fileParseErrorSupportMsg file) $ fatal $ FileParseError (toString file) (toText (errorBundlePretty err))
    Right a -> pure a

-- | Read from a file as a byte string, parsing its contents
readContentsParserBS :: forall a sig m. (Has ReadFS sig m, Has Diagnostics sig m) => Parsec Void ByteString a -> Path Abs File -> m a
readContentsParserBS parser file = context ("Parsing file '" <> toText (toString file) <> "'") $ do
  contents <- readContentsBS file
  case runParser parser (toString file) contents of
    Left err -> errSupport (fileParseErrorSupportMsg file) $ fatal $ FileParseError (toString file) (toText (errorBundlePretty err))
    Right a -> pure a

-- | Read JSON from a file
readContentsJson :: (FromJSON a, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m a
readContentsJson file = context ("Parsing JSON file '" <> toText (toString file) <> "'") $ do
  contents <- readContentsBS file
  case eitherDecodeStrict contents of
    Left err -> errSupport (fileParseErrorSupportMsg file) $ fatal $ FileParseError (toString file) (toText err)
    Right a -> pure a

readContentsToml :: (Toml.Schema.FromValue a, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m a
readContentsToml file = context ("Parsing TOML file '" <> toText (toString file) <> "'") $ do
  contents <- readContentsText file
  case Toml.decode contents of
    Toml.Failure err -> errSupport (fileParseErrorSupportMsg file) $ fatal $ FileParseError (toString file) (toText $ show err)
    Toml.Success _ a -> pure a

-- | Read YAML from a file
readContentsYaml :: (FromJSON a, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m a
readContentsYaml file = context ("Parsing YAML file '" <> toText (toString file) <> "'") $ do
  contents <- readContentsBS file
  case decodeEither' contents of
    Left err -> errSupport (fileParseErrorSupportMsg file) $ fatal $ FileParseError (toString file) (toText $ prettyPrintParseException err)
    Right a -> pure a

-- | Read XML from a file
readContentsXML :: (FromXML a, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m a
readContentsXML file = context ("Parsing XML file '" <> toText (toString file) <> "'") $ do
  contents <- readContentsText file
  case parseXML contents of
    Left err -> errSupport (fileParseErrorSupportMsg file) $ fatal $ FileParseError (toString file) (xmlErrorPretty err)
    Right a -> pure a

type ReadFSIOC = SimpleC ReadFSF

runReadFSIO :: Has (Lift IO) sig m => ReadFSIOC m a -> m a
runReadFSIO = interpret $ \case
  ReadContentsBS' file -> do
    BS.readFile (toString file)
      `catchingIO` FileReadError (toString file)
  ReadContentsBSLimit' file limit -> do
    readContentsBSLimitIO file limit
      `catchingIO` FileReadError (toString file)
  ReadContentsText' file -> do
    (decodeUtf8 <$> BS.readFile (toString file))
      `catchingIO` FileReadError (toString file)
  ReadRedactedContentsBS' file -> do
    mapRight Redacted
      <$> BS.readFile (toString file)
        `catchingIO` FileReadError (toString file)
  ReadRedactedContentsBSLimit' file limit -> do
    mapRight Redacted
      <$> readContentsBSLimitIO file limit
        `catchingIO` FileReadError (toString file)
  ReadRedactedContentsText' file -> do
    mapRight Redacted
      <$> (decodeUtf8 <$> BS.readFile (toString file))
        `catchingIO` FileReadError (toString file)
  ResolveFile' dir path -> do
    PIO.resolveFile dir (toString path)
      `catchingIO` ResolveError (toString dir) (toString path)
  ResolveDir' dir path -> do
    PIO.resolveDir dir (toString path)
      `catchingIO` ResolveError (toString dir) (toString path)
  ListDir dir -> do
    PIO.listDir dir
      `catchingIO` ListDirError (toString dir)
  GetIdentifier dir -> do
    (extractIdentifier <$> Posix.getFileStatus (toString dir))
      `catchingIO` FileReadError (toString dir)
    where
      extractIdentifier :: Posix.FileStatus -> DirID
      extractIdentifier status =
        let (CIno fileID) = Posix.fileID status
            (CDev devID) = Posix.deviceID status
         in DirID{dirFileID = toInteger fileID, dirDeviceID = toInteger devID}
  ResolvePath root path -> do
    let fullpath = toString root FP.</> path
    stat <- Posix.getFileStatus fullpath `catchingIO` ResolveError (toString root) path
    pure (stat >>= identifyPath fullpath)
  GetCurrentDir -> do
    PIO.getCurrentDir
      `catchingIO` CurrentDirError
  -- NB: these never throw
  DoesFileExist file -> sendIO (Directory.doesFileExist (fromSomeFile file))
  DoesDirExist dir -> sendIO (Directory.doesDirectoryExist (fromSomeDir dir))

identifyPath :: FilePath -> Posix.FileStatus -> Either ReadFSErr SomePath
identifyPath path stat = case (isDirectory stat, isRegularFile stat) of
  (True, False) -> first mangle (SomeDir <$> parseSomeDir path)
  (False, True) -> first mangle (SomeFile <$> parseSomeFile path)
  (False, False) -> Left $ NotDirOrFile path
  (True, True) -> Left $ UndeterminableFileType path
  where
    mangle :: E.SomeException -> ReadFSErr
    mangle = FileReadError path . showT

readContentsBSLimitIO :: SomeBase File -> Int -> IO ByteString
readContentsBSLimitIO file limit = withFile (fromSomeFile file) ReadMode $ \handle -> BS.hGetSome handle limit

catchingIO :: Has (Lift IO) sig m => IO a -> (Text -> ReadFSErr) -> m (Either ReadFSErr a)
catchingIO io mangle = (Right <$> sendIO io) `safeCatch` (\(e :: E.IOException) -> pure . Left . mangle . toText $ show e)
