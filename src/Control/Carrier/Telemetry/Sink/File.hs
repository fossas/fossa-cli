{-# LANGUAGE TemplateHaskell #-}

module Control.Carrier.Telemetry.Sink.File (sinkTelemetryToFile, fossaTelemetryDebugFile) where

import App.Fossa.DebugDir (globalDebugDirRef)
import Control.Carrier.Lift (Has, Lift, sendIO)
import Control.Carrier.Telemetry.Types (TelemetryRecord)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (
  Config (
    Config,
    confCompare,
    confIndent,
    confNumFormat,
    confTrailingNewline
  ),
  Indent (Spaces),
  NumberFormat (Generic),
  encodePretty',
 )
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (readIORef)
import Data.String.Conversion (toString)
import Path (File, Path, Rel, mkRelFile, (</>))
import Path.IO (getCurrentDir)

fossaTelemetryDebugFile :: Path Rel File
fossaTelemetryDebugFile = $(mkRelFile "fossa.telemetry.json")

-- | Write telemetry record to file.
--
-- File is written to debug directory if available, otherwise current working directory.
-- File is named @fossaTelemetryDebugFile@.
sinkTelemetryToFile :: Has (Lift IO) sig m => TelemetryRecord -> m ()
sinkTelemetryToFile record = do
  -- Check if we have a debug directory set
  maybeDebugDir <- sendIO $ readIORef globalDebugDirRef
  filePath <- case maybeDebugDir of
    Just debugDir -> do
      -- Write to debug directory
      pure $ debugDir <> "/fossa.telemetry.json"
    Nothing -> do
      -- Fall back to current directory
      currentDir <- sendIO getCurrentDir
      pure $ toString $ currentDir </> fossaTelemetryDebugFile
  sendIO $ encodeFile' filePath record

encodeFile' :: forall a. ToJSON a => FilePath -> a -> IO ()
encodeFile' path val =
  LazyByteString.writeFile path $
    encodePretty'
      Config
        { confIndent = Spaces 2
        , confCompare = compare
        , confNumFormat = Generic
        , confTrailingNewline = False
        }
      val
