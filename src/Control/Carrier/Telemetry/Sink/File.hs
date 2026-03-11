{-# LANGUAGE TemplateHaskell #-}

module Control.Carrier.Telemetry.Sink.File (sinkTelemetryToFile, fossaTelemetryDebugFile) where

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
import Data.IORef (IORef, readIORef)
import Data.String.Conversion (toString)
import Path (File, Path, Rel, mkRelFile)
import Path qualified as P
import Path.IO (getCurrentDir)
import System.FilePath ((</>))

fossaTelemetryDebugFile :: Path Rel File
fossaTelemetryDebugFile = $(mkRelFile "fossa.telemetry.json")

-- | Write telemetry record to file.
--
-- File is written to debug directory if available, otherwise current working directory.
-- File is named @fossaTelemetryDebugFile@.
-- We will have a debug directory if the `--debug` flag is used
-- We will still write the telemetry file to the current working directory if the `--debug` flag is not used
-- but `FOSSA_TELEMETRY_DEBUG` environment variable is set
sinkTelemetryToFile :: Has (Lift IO) sig m => IORef (Maybe FilePath) -> TelemetryRecord -> m ()
sinkTelemetryToFile debugDirRef record = do
  maybeDebugDir <- sendIO $ readIORef debugDirRef
  filePath <- case maybeDebugDir of
    Just debugDir -> do
      pure $ debugDir </> "fossa.telemetry.json"
    Nothing -> do
      currentDir <- sendIO getCurrentDir
      pure $ toString $ currentDir P.</> fossaTelemetryDebugFile
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
