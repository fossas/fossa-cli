{-# LANGUAGE FlexibleContexts #-}

module Strategy.Archive.RPM
  ( extractRpm
  )
where

import qualified Codec.RPM.Conduit as RPM
import Conduit
import Control.Effect.Lift
import Control.Exception (throwIO)
import Control.Monad.Except
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.CPIO as CPIO
import Path
import Prelude
import qualified Path.IO as PIO

extractRpm :: Has (Lift IO) sig m => Path Abs Dir -> Path Abs File -> m ()
extractRpm dir rpmFile = do
  res <-
    sendIO . runResourceT . runExceptT . runConduit $
      readRPMEntries rpmFile .| filterC (not . CPIO.isEntryDirectory) .| sinkDir
  case res of
    Left parseErr -> sendIO . throwIO $ parseErr
    Right () -> pure ()
  where
    -- morally: :: Path b File -> ConduitT i CPIO.Entry m ()
    -- (has a couple of additional constraints, e.g., MonadError for attoparsec ParseError)
    readRPMEntries file = sourceFileBS (toFilePath file) .| RPM.parseRPMC .| RPM.payloadContentsC
    --
    sinkDir :: MonadIO m => ConduitT CPIO.Entry o m ()
    sinkDir = mapM_C $ \entry -> do
      let filepath = BS8.unpack $ CPIO.cpioFileName entry

      -- explicitly ignore absolute paths
      case parseRelFile filepath of
        Nothing -> pure ()
        Just filepath' -> do
          liftIO . PIO.ensureDir $ (dir </> parent filepath')
          liftIO . BS.writeFile (fromAbsFile (dir </> filepath')) . BL.toStrict $ CPIO.cpioFileData entry
