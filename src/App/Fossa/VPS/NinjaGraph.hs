{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.NinjaGraph (
  ninjaGraphMain,
  NinjaGraphCmdOpts (..),
  scanNinjaDeps,
) where

import App.Fossa.ProjectInference
import App.Fossa.VPS.Scan.Core
import App.Fossa.VPS.Scan.ScotlandYard
import App.Fossa.VPS.Types
import App.Types (BaseDir (..), NinjaGraphCLIOptions (..), OverrideProject (..), ProjectRevision (..))
import App.Util (validateDir)
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Control.Effect.Lift (Lift, sendIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Effect.Exec
import Effect.Logger hiding (line)
import Effect.ReadFS
import Fossa.API.Types (ApiOpts)
import Path
import System.FilePath qualified as FP
import System.Process.Typed as PROC

data NinjaGraphCmdOpts = NinjaGraphCmdOpts
  { ninjaCmdBasedir :: FilePath
  , ninjaCmdNinjaGraphOpts :: NinjaGraphOpts
  }

data NinjaGraphError
  = ErrorRunningNinja Text
  | NoNinjaDepsStartLineFound
  | NoNinjaDepsEndLineFound
  | NinjaDepsParseError
  deriving (Eq, Ord, Show)

instance ToDiagnostic NinjaGraphError where
  renderDiagnostic = \case
    ErrorRunningNinja err -> "Error while running Ninja: " <> pretty err
    NoNinjaDepsStartLineFound -> "The output of \"ninja -t deps\" did not contain the \"Starting ninja...\" line"
    NoNinjaDepsEndLineFound -> "The output of \"ninja -t deps\" did not contain the \"build completed successfully\" line"
    NinjaDepsParseError -> "There was an error while parsing the output of \"ninja -t deps\""

data NinjaParseState = Starting | Parsing | Complete | Error

ninjaGraphMain :: ApiOpts -> Severity -> OverrideProject -> NinjaGraphCLIOptions -> IO ()
ninjaGraphMain apiOpts logSeverity overrideProject NinjaGraphCLIOptions{..} = do
  BaseDir basedir <- validateDir ninjaBaseDir

  withDefaultLogger logSeverity . logWithExit_ . runReadFSIO . runExecIO $ do
    ProjectRevision{..} <- mergeOverride overrideProject <$> (inferProjectFromVCS basedir <||> inferProjectDefault basedir)
    let ninjaGraphOpts = NinjaGraphOpts apiOpts ninjaDepsFile ninjaLunchTarget ninjaScanId projectName ninjaBuildName

    ninjaGraphInner basedir apiOpts ninjaGraphOpts

ninjaGraphInner :: (Has Logger sig m, Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs Dir -> ApiOpts -> NinjaGraphOpts -> m ()
ninjaGraphInner = getAndParseNinjaDeps

getAndParseNinjaDeps :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) => Path Abs Dir -> ApiOpts -> NinjaGraphOpts -> m ()
getAndParseNinjaDeps dir apiOpts ninjaGraphOpts@NinjaGraphOpts{..} = do
  ninjaDepsContents <- runReadFSIO . runExecIO $ getNinjaDeps dir ninjaGraphOpts
  graph <- scanNinjaDeps ninjaDepsContents
  SherlockInfo{..} <- getSherlockInfo ninjaFossaOpts
  let locator = createLocator ninjaProjectName sherlockOrgId
      syOpts = ScotlandYardNinjaOpts locator sherlockOrgId ninjaGraphOpts
  _ <- uploadBuildGraph apiOpts syOpts graph
  pure ()

-- If the path to an already generated ninja_deps file was passed in (with the --ninjadeps arg), then
-- read that file to get the ninja deps. Otherwise, generate it with
-- NINJA_ARGS="-t deps" make
getNinjaDeps :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m, Has (Lift IO) sig m) => Path Abs Dir -> NinjaGraphOpts -> m ByteString
getNinjaDeps baseDir opts@NinjaGraphOpts{..} =
  case ninjaGraphNinjaPath of
    Nothing -> BL.toStrict <$> generateNinjaDeps baseDir opts
    Just ninjaPath -> readNinjaDepsFile ninjaPath

scanNinjaDeps :: (Has Diagnostics sig m) => ByteString -> m [DepsTarget]
scanNinjaDeps ninjaDepsContents = map correctedTarget <$> ninjaDeps
  where
    ninjaDeps = parseNinjaDeps ninjaDepsContents

readNinjaDepsFile :: (Has Logger sig m, Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => FilePath -> m ByteString
readNinjaDepsFile ninjaPath = do
  logDebug . pretty $ "reading ninja deps from " ++ ninjaPath
  path <- sendIO $ parseAbsFile ninjaPath
  readContentsBS path

generateNinjaDeps :: (Has Logger sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => Path Abs Dir -> NinjaGraphOpts -> m BL.ByteString
generateNinjaDeps baseDir NinjaGraphOpts{..} = do
  logDebug . pretty $ "Generating ninja deps with this command: " ++ commandString
  (exitcode, stdout, stderr) <- sendIO $ PROC.readProcess (setWorkingDir (fromAbsDir baseDir) (PROC.shell commandString))
  case (exitcode, stdout, stderr) of
    (ExitSuccess, _, _) -> pure stdout
    (_, _, err) -> fatal (ErrorRunningNinja (toText (show err)))
  where
    commandString = case lunchTarget of
      Nothing -> "cd " ++ show baseDir ++ " && NINJA_ARGS=\"-t deps\" make"
      Just lunch -> "cd " ++ show baseDir ++ " && source ./build/envsetup.sh && lunch " ++ toString lunch ++ " && NINJA_ARGS=\"-t deps\" make"

correctedTarget :: DepsTarget -> DepsTarget
correctedTarget target@DepsTarget{targetDependencies = []} =
  target
correctedTarget target@DepsTarget{targetDependencies = [singleDep]} =
  target{targetDependencies = [], targetInputs = [singleDep]}
correctedTarget target@DepsTarget{targetDependencies = (firstDep : remainingDeps)} =
  fromMaybe (target{targetInputs = [firstDep], targetDependencies = remainingDeps}) (correctTargetWithLeadingTxtDeps target)

-- There are cases where the first N dependencies are .txt files and do not match the basename
-- of the target, and the N+1th dependency is a non-.txt file and matches the basename of
-- the target. In this case, the N+1th dependency is the correct input file
-- E.g., in this case we want the input to be "system/bpf/bpfloader/BpfLoader.cpp":
-- out/soong/.intermediates/system/bpf/bpfloader/bpfloader/android_arm64_armv8-a_core/obj/system/bpf/bpfloader/BpfLoader.o: #deps 3, deps mtime 1583962500 (VALID)
--     external/compiler-rt/lib/cfi/cfi_blacklist.txt
--     build/soong/cc/config/integer_overflow_blacklist.txt
--     system/bpf/bpfloader/BpfLoader.cpp
--     bionic/libc/include/arpa/inet.h
correctTargetWithLeadingTxtDeps :: DepsTarget -> Maybe DepsTarget
correctTargetWithLeadingTxtDeps target =
  case (leadingTxtDeps, restOfDeps) of
    ([], _) -> Nothing
    (_, []) -> Nothing
    (_, firstNonTxtDep : remainingDeps) ->
      if firstNonTxtDepBasename == targetBasenameWithoutExt
        then Just corrected
        else Nothing
      where
        (firstNonTxtDepBasename, _) = splitBasenameExt $ dependencyPath firstNonTxtDep
        corrected = target{targetDependencies = leadingTxtDeps ++ remainingDeps, targetInputs = [firstNonTxtDep]}
  where
    splitBasenameExt :: Text -> (String, String)
    splitBasenameExt = FP.splitExtension . FP.takeFileName . toString

    depsPathIsTxtAndBasenameDoesNotMatch :: String -> DepsDependency -> Bool
    depsPathIsTxtAndBasenameDoesNotMatch targetBasename dep =
      depExt == ".txt" && depBasename /= targetBasename
      where
        (depBasename, depExt) = splitBasenameExt $ dependencyPath dep

    (targetBasenameWithoutExt, _) = splitBasenameExt $ targetPath target
    (leadingTxtDeps, restOfDeps) = span (depsPathIsTxtAndBasenameDoesNotMatch targetBasenameWithoutExt) $ targetDependencies target

parseNinjaDeps :: (Has Diagnostics sig m) => ByteString -> m [DepsTarget]
parseNinjaDeps ninjaDepsLines =
  case finalState of
    Complete -> pure $ reverse reversedDependenciesResults
    Starting -> fatal NoNinjaDepsStartLineFound
    Parsing -> fatal NoNinjaDepsEndLineFound
    Error -> fatal NinjaDepsParseError
  where
    newLine = BS.head "\n" -- This is gross, but I couldn't get "BS.split '\n' ninjaDepsLines" to work
    nLines = BS.split newLine ninjaDepsLines
    (finalState, results) = foldl parseNinjaLine (Starting, []) nLines
    reversedDependenciesResults = map reverseDependencies results

-- The dependencies are reversed because we were consing them onto the
-- beginning of the array. Fix that here.
reverseDependencies :: DepsTarget -> DepsTarget
reverseDependencies target =
  target{targetDependencies = reverse deps}
  where
    deps = targetDependencies target

-- You can see a sample ninja deps file in test/App/VPSScan/testdata/small-ninja-deps
-- It starts with a preamble, which ends with a line that looks like
--
-- Starting ninja...
--
-- After the preamble there's the body.
-- The body itself consists of three types of lines:
-- blank lines, which are ignored
-- lines that start with a non-space character, which are target lines.
-- lines that start with spaces, which are dependency lines.
--
-- Target lines look like this:
--
-- out/target/product/coral/obj/JAVA_LIBRARIES/wifi-service_intermediates/dexpreopt.zip: #deps 2, deps mtime 1583991124 (VALID)
--
-- We just want the target, which is everything before the ": #deps"
--
-- Dependency lines are just four spaces followed by a path to a file. Like this:
--
--   device/google/coral/gpt-utils/gpt-utils.cpp
--
-- Again, we are only interested in the path, so we just strip off the leading spaces and return that.
--
-- The body ends with a line that looks like
-- [0;32m#### build completed successfully (20 seconds) ####[00m

parseNinjaLine :: ((NinjaParseState, [DepsTarget]) -> ByteString -> (NinjaParseState, [DepsTarget]))
parseNinjaLine (state, targets) line =
  case state of
    Starting ->
      if line == "Starting ninja..."
        then (Parsing, [])
        else (Starting, [])
    Parsing ->
      actuallyParseLine line targets
    Complete ->
      (Complete, targets)
    -- This should never happen
    _ -> (Error, targets)

actuallyParseLine :: ByteString -> [DepsTarget] -> (NinjaParseState, [DepsTarget])
-- ignore empty lines
actuallyParseLine "" targets =
  (Parsing, targets)
actuallyParseLine line []
  -- error if you're trying to add a dependency and there are no targets yet
  -- or if you reach the end of the file and no targets have been found
  | BS.isPrefixOf " " line || BS.isInfixOf "build completed successfully" line =
    (Error, [])
  -- Add the first target
  | otherwise =
    (Parsing, [newDepsTarget])
  where
    newDepsTarget = targetFromLine line
actuallyParseLine line (currentDepsTarget : restOfDepsTargets)
  -- The "build completed successfully" line signals that parsing is complete
  | BS.isInfixOf "build completed successfully" line =
    (Complete, currentDepsTarget : restOfDepsTargets)
  -- Lines starting with a space add a new dep to the current target
  | BS.isPrefixOf " " line =
    (Parsing, updatedDepsTarget : restOfDepsTargets)
  -- Lines starting with a non-blank char are new targets
  | otherwise =
    (Parsing, newDepsTarget : currentDepsTarget : restOfDepsTargets)
  where
    newDepsTarget = targetFromLine line
    updatedDepsTarget = addDepToDepsTarget currentDepsTarget line

targetFromLine :: ByteString -> DepsTarget
targetFromLine line =
  DepsTarget (decodeUtf8 tar) [] [] Nothing
  where
    (tar, _) = BS.breakSubstring ": #deps" line

addDepToDepsTarget :: DepsTarget -> ByteString -> DepsTarget
addDepToDepsTarget target line =
  target{targetDependencies = newDep : currentDeps}
  where
    currentDeps = targetDependencies target
    newDep = parseDepLine line

parseDepLine :: ByteString -> DepsDependency
parseDepLine line =
  DepsDependency (decodeUtf8 path) componentName hasDeps
  where
    path = stripLeadingSpace line
    componentName = Nothing -- TODO: get component name
    hasDeps = BS.isPrefixOf "out/" path

stripLeadingSpace :: ByteString -> ByteString
stripLeadingSpace = BS.dropWhile (\c -> c == BS.head " ")
