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
import Effect.ReadFS (ReadFS, doesFileExist, getCurrentDir, runReadFSIO)
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
initCommand = command "init" (info run $ progDesc "Creates example .fossa.yml and fossa-deps.yml file")
  where
    run :: Parser (IO ())
    run = pure $ withTelemetry . runStack . withDefaultLogger SevInfo . logWithExit_ . runReadFSIO $ runInit

runInit :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has ReadFS sig m, Has Logger sig m) => m ()
runInit = do
  dir <- getCurrentDir
  mkFossaYml dir
  mkFossaDeps dir

mkFossaYml :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has ReadFS sig m, Has Logger sig m) => Path Abs Dir -> m ()
mkFossaYml baseDir = do
  (fossaYml, fossaYmlExist) <- checkFile (baseDir </> fossaYmlFile)
  (fossaYaml, fossaYamlExist) <- checkFile (baseDir </> fossaYamlFile)

  case (fossaYmlExist, fossaYamlExist) of
    (True, _) -> fileAlreadyExists fossaYml
    (_, True) -> fileAlreadyExists fossaYaml
    _ -> do
      sendIO $ BS.writeFile (toFilePath fossaYml) exampleFossaYml
      logInfo . pretty $ "Created Example Configuration File: " <> (toFilePath fossaYml)

mkFossaDeps :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has ReadFS sig m, Has Logger sig m) => Path Abs Dir -> m ()
mkFossaDeps baseDir = do
  (fossaDepsYml, fossaDepsYmlExist) <- checkFile (baseDir </> fossaDepsYmlFile)
  (fossaDepsYaml, fossaDepsYamlExist) <- checkFile (baseDir </> fossaDepsYamlFile)
  (fossaDepsJson, fossaDepsJsonExist) <- checkFile (baseDir </> fossaDepsJsonFile)

  case (fossaDepsYmlExist, fossaDepsYamlExist, fossaDepsJsonExist) of
    (True, _, _) -> fileAlreadyExists fossaDepsYml
    (_, True, _) -> fileAlreadyExists fossaDepsYaml
    (_, _, True) -> fileAlreadyExists fossaDepsJson
    _ -> do
      sendIO $ BS.writeFile (toFilePath fossaDepsYml) exampleFossaDeps
      logInfo . pretty $ "Created Example fossa-deps File: " <> (toFilePath fossaDepsYml)

checkFile :: (Has Diagnostics sig m, Has ReadFS sig m) => Path Abs File -> m (Path Abs File, Bool)
checkFile path = do
  pathExists <- doesFileExist path
  pure (path, pathExists)

fileAlreadyExists :: Has Logger sig m => Path Abs File -> m ()
fileAlreadyExists path = logInfo . pretty $ "File: " <> toFilePath path <> " already exists, so skipping!"

exampleFossaYml :: ByteString
exampleFossaYml = $(embedFileIfExists "src/App/Fossa/Init/.fossa.yml")

exampleFossaDeps :: ByteString
exampleFossaDeps = $(embedFileIfExists "src/App/Fossa/Init/fossa-deps.yml")

fossaYmlFile :: Path Rel File
fossaYmlFile = $(mkRelFile ".fossa.yml")

fossaYamlFile :: Path Rel File
fossaYamlFile = $(mkRelFile ".fossa.yaml")

fossaDepsYmlFile :: Path Rel File
fossaDepsYmlFile = $(mkRelFile "fossa-deps.yml")

fossaDepsYamlFile :: Path Rel File
fossaDepsYamlFile = $(mkRelFile "fossa-deps.yaml")

fossaDepsJsonFile :: Path Rel File
fossaDepsJsonFile = $(mkRelFile "fossa-deps.json")
