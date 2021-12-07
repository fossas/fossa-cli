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

import Conduit (ConduitT, Void, await, decodeUtf8C, encodeUtf8C, filterC, linesUnboundedC, mapC, runConduitRes, sourceFile, yield, (.|))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Exception (IOException, Lift, catch)
import Control.Effect.Lift (sendIO)
import Crypto.Hash (Digest, HashAlgorithm, SHA256 (..), hashFinalize, hashInit, hashUpdate)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.ByteString qualified as B
import Data.String.Conversion (ToText (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Extra (breakOnAndRemove)
import Debug.Trace qualified as Debug
import Discovery.Walk (WalkStep (..), walk')
import Effect.ReadFS (ReadFS, contentIsBinary)
import Path (Abs, Dir, File, Path, toFilePath)

-- | Fingerprint deterministically idenfies a file and is derived from its content.
--
-- The type variable is:
--
--   @k@ - Kind, the kind of fingerprint computed.
--
-- For ease of implementation, the backing representation of a @Fingerprint@ instance is a @Base16@ encoded @Text@.
newtype Fingerprint k = Fingerprint Text

type role Fingerprint nominal

-- | Represents a 'Fingerprint' derived from the unmodified content of a file.
data Raw

-- | Represents a 'Fingerprint' derived from the content of a file with all C-style comments removed.
data CommentStripped

instance ToText (Fingerprint k) where
  toText (Fingerprint k) = k

instance ToJSON (Fingerprint k) where
  toJSON = toJSON . toText

-- | Represents the result of running all fingerprinting implementations on a file.
data Combined = Combined
  { combinedRaw :: Fingerprint Raw
  , combinedCommentStripped :: Maybe (Fingerprint CommentStripped)
  }

instance ToJSON Combined where
  toJSON Combined{..} =
    object
      [ "sha_256" .= toText combinedRaw
      , "comment_stripped:sha_256" .= fmap toText combinedCommentStripped
      ]

encodeFingerprint :: Digest hash -> Fingerprint t
encodeFingerprint = Fingerprint . toText . show

-- | Hashes a stream of 'B.ByteString'@s@ and creates a digest @d@.
-- Adapted from @sinkHash@ in https://hackage.haskell.org/package/cryptonite-conduit-0.2.2/docs/src/Crypto-Hash-Conduit.html
sinkHash :: (Monad m, HashAlgorithm hash) => ConduitT B.ByteString Void m (Digest hash)
sinkHash = sink hashInit
  where
    sink ctx = do
      b <- await
      case b of
        Nothing -> return $! hashFinalize ctx
        Just bs -> sink $! hashUpdate ctx bs

-- | Hashes the whole contents of the given file in constant memory.
-- Adapted from @hashFile@ in https://hackage.haskell.org/package/cryptonite-conduit-0.2.2/docs/src/Crypto-Hash-Conduit.html
hashFile :: (Has (Lift IO) sig m, Has Diagnostics sig m, HashAlgorithm hash) => FilePath -> m (Digest hash)
hashFile fp =
  sendIO
    ( runConduitRes $
        sourceFile fp
          .| decodeUtf8C -- Decode to text
          .| normalizeLineEnding -- Convert file to \n only
          .| encodeUtf8C -- Encode back to bytes
          .| sinkHash -- Hash the result
    )
    `catch` (\(e :: IOException) -> fatalText ("unable to hash file: " <> toText (show e)))

hashFileCommentStripped :: (Has (Lift IO) sig m, Has Diagnostics sig m, HashAlgorithm hash) => FilePath -> m (Digest hash)
hashFileCommentStripped file =
  sendIO
    ( runConduitRes $
        sourceFile file -- Read from the file
          .| decodeUtf8C -- Decode to text
          .| normalizeLineEnding -- Convert file to \n only
          .| basicCStyleCommentStripC -- Strip comments
          .| trace
          .| encodeUtf8C -- Encode back to bytes
          .| sinkHash -- Hash the result
    )
    `catch` (\(e :: IOException) -> fatalText ("unable to hash file: " <> toText (show e)))
  where
    trace = do
      chunk <- await
      case chunk of
        Nothing -> pure ()
        Just l -> do
          yield $ Debug.trace ("-> " ++ show l) l
          trace

fingerprintRaw :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m (Fingerprint Raw)
fingerprintRaw file = do
  (fp :: Digest SHA256) <- hashFile $ toFilePath file
  pure $ encodeFingerprint fp

fingerprintContentsRaw :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => Path Abs Dir -> m [Fingerprint Raw]
fingerprintContentsRaw = walk' $ \_ _ files -> do
  fps <- traverse fingerprintRaw files
  pure (fps, WalkContinue)

fingerprintCommentStripped :: (Has ReadFS sig m, Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m (Maybe (Fingerprint CommentStripped))
fingerprintCommentStripped file = contentIsBinary file >>= doFingerprint
  where
    doFingerprint True = pure Nothing -- Don't attempt to comment strip binary files
    doFingerprint False = do
      (fp :: Digest SHA256) <- hashFileCommentStripped $ toFilePath file
      pure . Just $ encodeFingerprint fp

fingerprint :: (Has ReadFS sig m, Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m Combined
fingerprint file =
  Combined
    <$> fingerprintRaw file
    <*> fingerprintCommentStripped file

-- | Windows git implementations typically add carriage returns before each newline when checked out.
-- However, crawlers were run on Linux, so aren't expecting files to have carriage returns.
-- Convert CRLF -> LF. While this does cause a hash mismatch on files that legitimately have CRLF endings, this way of doing it is believed to result in less misses.
normalizeLineEnding :: Monad m => ConduitT Text Text m ()
normalizeLineEnding =
  linesUnboundedC .| do
    chunk <- await
    case chunk of
      Nothing -> pure ()
      Just c -> do
        yield $ Text.replace "\r\n" "\n" c
        normalizeLineEnding

-- | This implementation is based on the comment strip logic from the internal logic used when crawling OSS components.
-- It is very basic:
--
-- * Only works for C-style comments
-- * Only catches @\\n@ newlines
-- * Doesn't handle edge cases (escaped comments for example)
-- * Also omits any blank lines
--
-- Despite these drawbacks, we have to reimplement it the same way so that fingerprints line up correctly in the VSI analysis service.
basicCStyleCommentStripC :: Monad m => ConduitT Text Text m ()
basicCStyleCommentStripC =
  linesUnboundedC
    .| process
    .| mapC Text.strip
    .| filterC (not . Text.null)
    .| bufferedNewline Nothing
  where
    -- For compatibility with the original version of this function we can't write a newline after the final line in the output, but we want newlines otherwise.
    -- As we read through the input stream, instead of writing lines directly we'll buffer one at a time.
    -- This way we can delay the decision of whether to write a trailing newline until we know if we're at the end of the input.
    bufferedNewline buf = do
      chunk <- await
      case chunk of
        -- Now that we're done reading the input stream, if there's a buffered output line write it *without a trailing newline*.
        -- This is to maintain compatibility with the original version of this function.
        -- We have to keep this compatible, because all of our fingerprint corpus relies on how this fingerprint function works.
        Nothing -> case buf of
          Nothing -> pure ()
          Just bufferedLine -> do
            yield bufferedLine
            bufferedNewline Nothing

        -- Here, we know we have a new line coming down the pipe.
        -- If we had a previous line buffered, write it with a newline.
        -- If not, store this new line in the buffer first.
        Just line -> case buf of
          Nothing -> bufferedNewline (Just line)
          Just bufferedLine -> do
            yield $ bufferedLine <> "\n"
            bufferedNewline (Just line)

    processInComment = do
      chunk <- await
      case chunk of
        Nothing -> pure ()
        Just line -> case breakOnAndRemove "*/" line of
          Nothing -> processInComment
          Just (_, lineAfterComment) -> do
            yield lineAfterComment
            process

    process = do
      chunk <- await
      case chunk of
        Nothing -> pure ()
        Just line -> case breakOnAndRemove "/*" line of
          Nothing -> do
            yield $ fst (Text.breakOn "//" line)
            process
          Just (lineBeforeComment, _) -> do
            yield lineBeforeComment
            processInComment
