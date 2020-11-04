{-# LANGUAGE FlexibleContexts #-}

module Discovery.Archive.RPM
  ( extractRpm,
  )
where

import qualified Codec.RPM.Conduit as RPM
import qualified Codec.RPM.Tags as Tags
import qualified Codec.RPM.Types as RPMTypes
import Conduit
import Control.Effect.Lift
import Control.Exception (throwIO)
import Control.Monad.Except
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.CPIO as CPIO
import qualified Data.Conduit.Lzma as Lzma
import qualified Data.Conduit.Zlib as Zlib
import Data.Foldable (asum)
import Path
import qualified Path.IO as PIO
import Prelude

extractRpm :: Has (Lift IO) sig m => Path Abs Dir -> Path Abs File -> m ()
extractRpm dir rpmFile = do
  res <-
    sendIO . runResourceT . runExceptT . runConduit $ do
      sourceFileBS (toFilePath rpmFile) .| RPM.parseRPMC .| awaitForever (extractEntries dir)

  case res of
    Left parseErr -> sendIO . throwIO $ parseErr
    Right () -> pure ()

-- | Extract RPM entries to a directory
extractEntries :: (PrimMonad m, MonadThrow m, MonadIO m) => Path Abs Dir -> RPMTypes.RPM -> ConduitT i o m ()
extractEntries dir rpm = yield rpm .| RPM.payloadC .| decompressorFor rpm .| CPIO.readCPIO .| filterC (not . CPIO.isEntryDirectory) .| sinkDir dir

-- | Extract each incoming CPIO entry on the Conduit to a directory
sinkDir :: MonadIO m => Path Abs Dir -> ConduitT CPIO.Entry o m ()
sinkDir dir = mapM_C $ \entry -> do
  let filepath = BS8.unpack $ CPIO.cpioFileName entry

  -- explicitly ignore absolute paths
  case parseRelFile filepath of
    Nothing -> pure ()
    Just filepath' -> do
      liftIO . PIO.ensureDir $ (dir </> parent filepath')
      liftIO . BS.writeFile (fromAbsFile (dir </> filepath')) . BL.toStrict $ CPIO.cpioFileData entry

-- | Choose the correct decompressor for a given RPM.
-- By default, we choose lzma: this is what the reference implementation does.
decompressorFor :: (PrimMonad m, MonadThrow m, MonadIO m) => RPMTypes.RPM -> ConduitT BS.ByteString BS.ByteString m ()
decompressorFor rpm =
  case getCompressor rpm of
    Nothing -> Lzma.decompress Nothing -- default to lzma
    Just GZIP -> Zlib.ungzip
    Just LZMA -> Lzma.decompress Nothing

data Compressor
  = LZMA
  | GZIP
  deriving (Eq, Ord, Show)

-- | There are two types of supported compressors in RPM files: "gzip" and "lzma"
-- We assume the RPM is well-formed, and pick the first tag we see.
getCompressor :: RPMTypes.RPM -> Maybe Compressor
getCompressor = asum . map go . concatMap RPMTypes.headerTags . RPMTypes.rpmHeaders
  where
    go :: Tags.Tag -> Maybe Compressor
    go (Tags.PayloadCompressor "gzip") = Just GZIP
    go (Tags.PayloadCompressor "lzma") = Just LZMA
    go _ = Nothing
