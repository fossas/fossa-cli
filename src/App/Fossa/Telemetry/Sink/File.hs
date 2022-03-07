{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Telemetry.Sink.File (sinkTelemetryToFile, fossaTelemetryDebugFile) where

import App.Fossa.Telemetry.Types (TelemetryRecord)
import Control.Carrier.Lift (Has, Lift, sendIO)
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
import Path (File, Path, Rel, fromAbsFile, mkRelFile, (</>))
import Path.IO (getCurrentDir)

fossaTelemetryDebugFile :: Path Rel File
fossaTelemetryDebugFile = $(mkRelFile "fossa.telemetry.json")

-- | Write telemetry record to file.
--
-- File is written in current working directory,
-- under @fossaTelemetryDebugFile@ filename.
sinkTelemetryToFile :: Has (Lift IO) sig m => TelemetryRecord -> m ()
sinkTelemetryToFile record = do
  currentDir <- sendIO getCurrentDir
  sendIO $ encodeFile' (fromAbsFile $ currentDir </> fossaTelemetryDebugFile) record

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
