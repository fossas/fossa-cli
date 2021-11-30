module App.Fossa.VSI.Fingerprint (
  fingerprintRaw,
  fingerprintContentsRaw,
) where

import App.Fossa.VSI.IAT.Types (Fingerprint (Fingerprint))
import Conduit (ConduitT, Void, await, runConduitRes, sourceFile, (.|))
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Exception (IOException, Lift, catch)
import Control.Effect.Lift (sendIO)
import Crypto.Hash (
  Digest,
  HashAlgorithm,
  SHA256,
  hashFinalize,
  hashInit,
  hashUpdate,
 )
import Data.ByteString qualified as B
import Data.String.Conversion (ToText (..))
import Discovery.Walk (WalkStep (..), walk')
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, File, Path, toFilePath)

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

fingerprintRaw :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m Fingerprint
fingerprintRaw file = do
  (fp :: Digest SHA256) <- hashFile $ toFilePath file
  let base16 = toText . show $ fp
  pure (Fingerprint base16)

fingerprintContentsRaw :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => Path Abs Dir -> m [Fingerprint]
fingerprintContentsRaw = walk' $ \_ _ files -> do
  fps <- traverse fingerprintRaw files
  pure (fps, WalkContinue)
