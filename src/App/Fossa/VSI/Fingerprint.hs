{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSI.Fingerprint (
  fingerprintRaw,
  fingerprintContentsRaw,
  fingerprintCommentStripped,
  fingerprint,
  Fingerprint,
  Raw,
  CommentStripped,
  Combined,
) where

import Conduit (ConduitT, Void, await, runConduitRes, sourceFile, (.|))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Exception (IOException, Lift, catch)
import Control.Effect.Lift (sendIO)
import Crypto.Hash (Digest, HashAlgorithm, SHA256 (..), hashFinalize, hashInit, hashUpdate, hashWith)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.ByteString qualified as B
import Data.String.Conversion (ToText (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Discovery.Walk (WalkStep (..), walk')
import Effect.ReadFS (ReadFS, contentIsBinary, readContentsText)
import Path (Abs, Dir, File, Path, toFilePath)

-- | Fingerprint deterministically idenfies a file and is derived from its content.
--
-- The type variable is:
--
--   @k@ - Kind, the kind of fingerprint computed.
--
-- For ease of implementation, the backing representation of a @Fingerprint@ instance is a @Base16@ encoded @Text@.
newtype Fingerprint k = Fingerprint Text

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
  sendIO (runConduitRes (sourceFile fp .| sinkHash))
    `catch` (\(e :: IOException) -> fatalText ("unable to hash file: " <> toText (show e)))

fingerprintRaw :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m (Fingerprint Raw)
fingerprintRaw file = do
  (fp :: Digest SHA256) <- hashFile $ toFilePath file
  pure $ encodeFingerprint fp

fingerprintContentsRaw :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => Path Abs Dir -> m [Fingerprint Raw]
fingerprintContentsRaw = walk' $ \_ _ files -> do
  fps <- traverse fingerprintRaw files
  pure (fps, WalkContinue)

fingerprintCommentStripped :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Maybe (Fingerprint CommentStripped))
fingerprintCommentStripped file = contentIsBinary file >>= doFingerprint
  where
    doFingerprint True = pure Nothing -- Don't attempt to comment strip binary files
    doFingerprint False = do
      content <- readContentsText file
      pure . Just . encodeFingerprint . hashWith SHA256 . TE.encodeUtf8 $ basicCStyleCommentStrip content

fingerprint :: (Has ReadFS sig m, Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m Combined
fingerprint file =
  Combined
    <$> fingerprintRaw file
    <*> fingerprintCommentStripped file

-- | This implementation is based on the comment strip logic from the internal logic used when crawling OSS components.
-- It is very basic, only works for C-style comments and @\n@ newlines, and doesn't handle edge cases.
-- Despite these drawbacks, we have to reimplement it the same way so that fingerprints line up correctly in the VSI analysis service.
--
-- TODO: We should ideally move this to stream processing instead of buffering the whole file.
basicCStyleCommentStrip :: Text -> Text
basicCStyleCommentStrip content = Text.unlines . (process False) $ Text.lines content
  where
    breakOn' :: Text -> Text -> (Text, Text, Bool)
    breakOn' needle haystack = do
      let (before, remaining) = Text.breakOn needle haystack
      if remaining == Text.empty -- `remaining` includes the needle if found, so if it's empty it wasn't found
        then (before, Text.empty, False)
        else (before, Text.drop (Text.length needle) remaining, True)

    mergeLineWithRest :: Bool -> Text -> [Text] -> [Text]
    mergeLineWithRest inMultilineComment line remainingLines =
      if line == Text.empty
        then process inMultilineComment remainingLines
        else line : process inMultilineComment remainingLines

    process :: Bool -> [Text] -> [Text]
    process _ [] = []
    process True (line : remainingLines) = do
      let (_, lineAfterComment, foundCommentEnd) = breakOn' "*/" line
      mergeLineWithRest (not foundCommentEnd) lineAfterComment remainingLines
    process False (line : remainingLines) = do
      let (lineBeforeComment, _, foundCommentStart) = breakOn' "/*" line
      if foundCommentStart
        then mergeLineWithRest True lineBeforeComment remainingLines
        else fst (Text.breakOn line "//") : process False remainingLines -- Trim off any single line comment at the end of the line
