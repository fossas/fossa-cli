{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Init (
  initCommand,

  -- * testing only
  mkFossaYml,
  mkFossaDeps,
  exampleFossaYml,
  exampleFossaDeps,
) where

import Control.Algebra (Has)
import Control.Carrier.Diagnostics (Diagnostics, logWithExit_)
import Control.Carrier.Lift (sendIO)
import Control.Carrier.Stack (runStack)
import Control.Carrier.Telemetry (withTelemetry)
import Control.Effect.Lift (Lift)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed.Extra (embedFileIfExists)
import Effect.Logger (Logger, Severity (SevInfo), logInfo, pretty, withDefaultLogger)
import Effect.ReadFS (ReadFS, getCurrentDir, runReadFSIO)
import Options.Applicative (CommandFields, Mod, Parser, info, progDesc)
import Options.Applicative.Builder (command)
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  mkRelFile,
  toFilePath,
  (</>),
 )

initCommand :: Mod CommandFields (IO ())
initCommand = command "init" (info run $ progDesc "Creates .fossa.yml.example and fossa-deps.yml.example file")
  where
    run :: Parser (IO ())
    run = pure $ withTelemetry . runStack . withDefaultLogger SevInfo . logWithExit_ . runReadFSIO $ runInit

runInit :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has ReadFS sig m, Has Logger sig m) => m ()
runInit = do
  dir <- getCurrentDir
  mkFossaYml dir
  mkFossaDeps dir

mkFossaYml :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) => Path Abs Dir -> m ()
mkFossaYml baseDir = do
  let fossaYmlExample = baseDir </> fossaYmlFile
  sendIO $ BS.writeFile (toFilePath fossaYmlExample) exampleFossaYml
  logInfo . pretty $ "Wrote example configuration File: " <> (toFilePath fossaYmlExample)


mkFossaDeps :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) => Path Abs Dir -> m ()
mkFossaDeps baseDir = do
  let fossaDepsYml = baseDir </> fossaDepsYmlFile
  sendIO $ BS.writeFile (toFilePath fossaDepsYml) exampleFossaDeps
  logInfo . pretty $ "Wrote example fossa-deps File: " <> (toFilePath fossaDepsYml)


exampleFossaYml :: ByteString
exampleFossaYml = $(embedFileIfExists "src/App/Fossa/Init/.fossa.yml")

exampleFossaDeps :: ByteString
exampleFossaDeps = $(embedFileIfExists "src/App/Fossa/Init/fossa-deps.yml")

fossaYmlFile :: Path Rel File
fossaYmlFile = $(mkRelFile ".fossa.yml.example")

fossaDepsYmlFile :: Path Rel File
fossaDepsYmlFile = $(mkRelFile "fossa-deps.yml.example")
