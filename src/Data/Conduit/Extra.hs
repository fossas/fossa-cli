module Data.Conduit.Extra (
  sinkHash,
) where

import Conduit (ConduitT, Void, await)
import Crypto.Hash (Digest, HashAlgorithm, hashFinalize, hashInit, hashUpdate)
import Data.ByteString (ByteString)

-- | Hashes a stream of 'ByteString'@s@ and creates a digest @d@.
-- Adapted from @sinkHash@ in https://hackage.haskell.org/package/cryptonite-conduit-0.2.2/docs/src/Crypto-Hash-Conduit.html
sinkHash :: (Monad m, HashAlgorithm hash) => ConduitT ByteString Void m (Digest hash)
sinkHash = sink hashInit
  where
    sink ctx = do
      b <- await
      case b of
        Nothing -> pure $! hashFinalize ctx
        Just bs -> sink $! hashUpdate ctx bs
