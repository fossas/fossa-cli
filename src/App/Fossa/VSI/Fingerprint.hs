{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}

module App.Fossa.VSI.Fingerprint (
  fingerprintRaw,
  fingerprintContentsRaw,
  fingerprintCommentStripped,
  fingerprint,
  Fingerprint,
  Raw,
  CommentStripped,
  Combined (..),
) where

import Conduit (ConduitT, await, filterC, linesUnboundedAsciiC, mapC, runConduitRes, sourceFile, yield, (.|))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, fatalOnIOException)
import Control.Effect.Exception (Lift)
import Control.Effect.Lift (sendIO)
import Crypto.Hash (Digest, HashAlgorithm, SHA256 (..))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Conduit.Extra (sinkHash)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (ToText (..))
import Data.Text (Text)
import Data.Word8 (isSpace)
import Discovery.Walk (WalkStep (..), walk')
import Effect.ReadFS (ReadFS, contentIsBinary)
import Path (Abs, Dir, File, Path, toFilePath)
import Discovery.Filters (AllFilters)

-- | Fingerprint deterministically idenfies a file and is derived from its content.
--
-- The type variable is:
--
--   @k@ - Kind, the kind of fingerprint computed.
--
-- For ease of implementation, the backing representation of a @Fingerprint@ instance is a @Base16@ encoded @Text@.
newtype Fingerprint k = Fingerprint Text deriving (Show, Eq, ToJSON)

type role Fingerprint nominal

-- | Represents a 'Fingerprint' derived from the unmodified content of a file.
data Raw

-- | Represents a 'Fingerprint' derived from the content of a file with all C-style comments removed.
data CommentStripped

instance ToText (Fingerprint k) where
  toText (Fingerprint k) = k

-- | Represents the result of running all fingerprinting implementations on a file.
data Combined = Combined
  { combinedRaw :: Fingerprint Raw
  , combinedCommentStripped :: Maybe (Fingerprint CommentStripped)
  }
  deriving (Show, Eq)

instance ToJSON Combined where
  toJSON Combined{..} =
    object
      [ "sha_256" .= toText combinedRaw
      , "comment_stripped:sha_256" .= fmap toText combinedCommentStripped
      ]

encodeFingerprint :: Digest SHA256 -> Fingerprint t
encodeFingerprint = Fingerprint . toText . show

-- | Hashes the whole contents of the given file in constant memory.
hashBinaryFile :: (Has (Lift IO) sig m, Has Diagnostics sig m, HashAlgorithm hash) => FilePath -> m (Digest hash)
hashBinaryFile fp =
  context "as binary" $
    (fatalOnIOException "hash binary file") . sendIO . runConduitRes $
      sourceFile fp .| sinkHash

hashTextFileCommentStripped :: (Has (Lift IO) sig m, Has Diagnostics sig m, HashAlgorithm hash) => FilePath -> m (Digest hash)
hashTextFileCommentStripped file =
  (fatalOnIOException "hash text file comment stripped") . sendIO . runConduitRes $
    sourceFile file -- Read from the file
      .| basicCStyleCommentStripC -- Strip comments
      .| sinkHash -- Hash the result

hashTextFile :: (Has (Lift IO) sig m, Has Diagnostics sig m, HashAlgorithm hash) => FilePath -> m (Digest hash)
hashTextFile file =
  context "as text" $
    (fatalOnIOException "hash text file") . sendIO . runConduitRes $
      sourceFile file -- Read from the file
        .| linesUnboundedAsciiC -- Split into lines (for @stripCrLines@)
        .| stripCrLines -- Normalize CRLF -> LF
        .| mapC (<> "\n") -- Always append a newline here
        .| sinkHash -- Hash the result

fingerprintRaw :: (Has ReadFS sig m, Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m (Fingerprint Raw)
fingerprintRaw file = context "raw" $ contentIsBinary file >>= doFingerprint
  where
    doFingerprint isBinary = do
      let hasher = if isBinary then hashBinaryFile else hashTextFile
      fp <- hasher $ toFilePath file
      pure $ encodeFingerprint fp

fingerprintContentsRaw :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => Maybe AllFilters -> Path Abs Dir -> m [Fingerprint Raw]
fingerprintContentsRaw filters = walk' filters $ \_ _ files -> do
  fps <- traverse fingerprintRaw files
  pure (fps, WalkContinue)

fingerprintCommentStripped :: (Has ReadFS sig m, Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m (Maybe (Fingerprint CommentStripped))
fingerprintCommentStripped file = context "comment stripped" $ contentIsBinary file >>= doFingerprint
  where
    doFingerprint True = pure Nothing -- Don't attempt to comment strip binary files
    doFingerprint False = do
      fp <- hashTextFileCommentStripped $ toFilePath file
      pure . Just $ encodeFingerprint fp

fingerprint :: (Has ReadFS sig m, Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m Combined
fingerprint file =
  context "fingerprint combined" $
    Combined
      <$> fingerprintRaw file
      <*> fingerprintCommentStripped file

-- | Converts CRLF line endings into LF line endings.
-- Must run after a @ConuitT@ that converts an input stream into lines (for example 'linesUnboundedC').
--
-- Windows git implementations typically add carriage returns before each newline when checked out.
-- However, crawlers are run on Linux, so aren't expecting files to have carriage returns.
-- While this does cause a hash mismatch on files that legitimately have CRLF endings that weren't added by git, we believe this results in fewer mismatches.
stripCrLines :: Monad m => ConduitT ByteString ByteString m ()
stripCrLines = do
  chunk <- await
  case chunk of
    Nothing -> pure ()
    Just line -> do
      yield $ fromMaybe line (BS.stripSuffix "\r" line)
      stripCrLines

-- | This implementation is based on the comment strip logic from the internal logic used when crawling OSS components.
-- It is very basic:
--
-- * Only works for C-style comments
-- * Only catches @\\n@ newlines
-- * Doesn't handle edge cases (escaped comments for example)
-- * Also omits any blank lines
-- * Trims any trailing newline off the content
--
-- Despite these drawbacks, we have to reimplement it the same way so that fingerprints line up correctly in the VSI analysis service.
--
-- Uses @ByteString@ instead of @Text@ to replicate the functionality of the Go implementation of this logic.
basicCStyleCommentStripC :: Monad m => ConduitT ByteString ByteString m ()
basicCStyleCommentStripC =
  linesUnboundedAsciiC
    .| stripCrLines
    .| process
    .| mapC stripSpace
    .| filterC (not . BS.null)
    .| bufferedNewline Nothing
  where
    -- The original version of this function included newlines between each line but did not include a trailing newline, even when originally present in the file.
    -- We have to keep this compatible, because all of our fingerprint corpus relies on how this fingerprint function works.
    -- As we read through the input stream, instead of writing lines directly we'll buffer one at a time.
    -- This way we can delay the decision of whether to write a trailing newline until we know if we're at the end of the input.
    bufferedNewline buf = do
      chunk <- await
      case (chunk, buf) of
        -- First line lands here and is always buffered.
        (Just line, Nothing) -> bufferedNewline (Just line)
        -- All lines other than the last yield with a newline appended.
        -- This only happens when we know we have another line incoming.
        (Just incomingLine, Just bufferedLine) -> do
          yield $ bufferedLine <> "\n"
          bufferedNewline (Just incomingLine)
        -- No incoming line, so the buffered line is the last one.
        -- For compatibility, this line must not have a trailing newline appended.
        (Nothing, Just bufferedLine) -> yield bufferedLine
        -- All lines have been written, so just exit.
        -- Technically unreachable since we don't recurse after yielding the last line.
        (Nothing, Nothing) -> pure ()

    -- Throws away lines until we find a line with the literal @*/@.
    -- Once found, yields all the text *after* the literal and returns to the standard 'process' function.
    processInComment = do
      chunk <- await
      case chunk of
        Nothing -> pure ()
        Just line -> case breakSubstringAndRemove "*/" line of
          Nothing -> processInComment
          Just (_, lineAfterComment) -> do
            yield lineAfterComment
            process

    -- Yields lines that do not contain comments without modification.
    -- For lines which contain a single-line comment (@//@), yields only the text leading up to that comment (so @foo // comment@ becomes @foo @).
    -- For lines which contain the literal @/*@:
    -- - Yields the text leading up to the literal.
    -- - Enters the specialized 'processInComment' function.
    process = do
      chunk <- await
      case chunk of
        Nothing -> pure ()
        Just line -> case breakSubstringAndRemove "/*" line of
          Nothing -> do
            yield $ fst (BS.breakSubstring "//" line)
            process
          Just (lineBeforeComment, _) -> do
            yield lineBeforeComment
            processInComment

-- | Like 'BS.breakSubstring', but with two differences.
--
-- 1. This removes the text that was broken on:
--
-- > BS.breakSubstring "foo" "foobar" == ("", "foobar")
-- > breakSubstringAndRemove "foo" "foobar" == ("", "bar")
--
-- 2. If the substring was not found, the result is @Nothing@,
-- instead of one of the options being a blank @ByteString@.
breakSubstringAndRemove :: ByteString -> ByteString -> Maybe (ByteString, ByteString)
breakSubstringAndRemove needle haystack = do
  let (before, after) = BS.breakSubstring needle haystack
  if needle `BS.isPrefixOf` after
    then pure (before, BS.drop (BS.length needle) after)
    else Nothing

-- | Remove leading and trailing spaces.
stripSpace :: ByteString -> ByteString
stripSpace s = BS.dropWhileEnd isSpace $ BS.dropWhile isSpace s
