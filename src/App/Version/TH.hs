{-# LANGUAGE TemplateHaskell #-}
-- We disable overlapping-patterns in this module to handle the case statement
-- in `getCurrentTag`. GHC always thinks one of the two cases is redundant
-- because `tGitInfoCwdTry` is compiled by Template Haskell into a concrete
-- Right or Left. However, both branches are needed to handle different
-- compilation environments.
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module App.Version.TH (
  getCurrentTag,
  themisVersionQ,
) where

import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Control.Effect.Diagnostics (Diagnostics, fromEitherShow)
import Control.Effect.Exception (Exception (displayException), SomeException)
import Control.Exception.Safe (catchAny)
import Control.Monad (when)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Data.String.Conversion (decodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Versions (errorBundlePretty, semver)
import Diag.Result (Result (Success))
import Effect.Exec (
  AllowErr (Always),
  Command (..),
  Exec,
  Has,
  exec,
  runExecIO,
 )
import Effect.ReadFS (ReadFS, getCurrentDir, runReadFSIO)
import GitHash (giHash, tGitInfoCwdTry)
import Instances.TH.Lift ()
import Language.Haskell.TH (
  Code,
  Q,
  bindCode,
  bindCode_,
  joinCode,
 )
import Language.Haskell.TH.Syntax (
  Quasi (qAddDependentFile),
  reportWarning,
  runIO,
 )
import Path (parseRelFile)
import Path.IO (doesFileExist)
import System.Process.Typed (ExitCode (ExitFailure, ExitSuccess), readProcess)

gitTagPointCommand :: Text -> Command
gitTagPointCommand commit =
  Command
    { cmdName = "git"
    , cmdArgs = ["tag", "--points-at", commit]
    , cmdAllowErr = Always
    }

-- |Return the current tag iff it is a valid semver tag.
getCurrentTag :: Code Q (Maybe Text)
getCurrentTag = joinCode $ do
  case $$(tGitInfoCwdTry) of
    Right info -> do
      let commitHash = giHash info
      result <- runIO . runStack . runDiagnostics . runExecIO . runReadFSIO . getTags $ toText commitHash
      pure $ case result of
        Success _ tags -> filterTags tags
        err -> reportWarning (show err) `bindCode_` [||Nothing||]
    Left err -> pure $ reportWarning (show err) `bindCode_` [||Nothing||]

getTags :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => Text -> m [Text]
getTags hash = do
  dir <- getCurrentDir
  -- FIXME: boy it would be nice to not shell out during compilation.  So much that could go wrong.
  result <- exec dir $ gitTagPointCommand hash
  bsl <- fromEitherShow result

  pure . map Text.strip . Text.lines . decodeUtf8 $ BSL.toStrict bsl

{- We'd like to use this time to make sure we have tags when we build release
    versions.  However, 2 things make that difficult at the time of writing:

    * We don't know if we're in a GH PR or a Release build.
    * We only build releases by pushing tags, so we can't even trigger the error case

    For these reasons, we're ignoring the case where there is no tag.  Future enhancements
    should apply to the [] case of the filterTags function below.  Theoretically, we COULD
    do some IO to determine something about github, and execute using `runIO`.
-}
filterTags :: [Text] -> Code Q (Maybe Text)
filterTags [] = [||Nothing||]
filterTags [x] = validateSingleTag x
filterTags xs = reportWarning (toString multiTagMesg) `bindCode_` [||Nothing||]
  where
    multiTagMesg = header <> Text.intercalate ", " xs
    header = "Multiple tags defined at current commit: "

validateSingleTag :: Text -> Code Q (Maybe Text)
validateSingleTag tag = do
  let normalized = fromMaybe tag $ Text.stripPrefix "v" tag

  case semver normalized of
    Left err -> reportWarning (errorBundlePretty err) `bindCode_` [||Nothing||]
    Right _ -> [||Just normalized||]

themisVersionQ :: Code Q Text
themisVersionQ = do
  -- qAddDependentFile is meant to signal that when this file changes we should be rebuilt.
  -- cabal also has to be notified in spectrometer.cabal's extra-source-files.
  ( do
      case parseRelFile "vendor-bins/themis-cli" of
        Just path' -> do
          fileExists <- runIO $ doesFileExist path'
          when fileExists (qAddDependentFile "vendor-bins/themis-cli")
        Nothing -> pure ()
      runIO (catchAny readProcess' exceptionToText)
    )
    `bindCode` spliceVersion
  where
    spliceVersion output = do
      case output of
        Left _ -> [||""||]
        Right t ->
          let t' = Text.takeWhileEnd (/= ' ') $ Text.strip t in [||t'||]
    readProcess' :: (IO (Either String Text))
    readProcess' = do
      processOutput <$> readProcess "vendor-bins/themis-cli --version"

processOutput :: (ExitCode, BL.ByteString, BL.ByteString) -> (Either String Text)
processOutput (ExitFailure _, _, _) = Left ""
processOutput (ExitSuccess, stdout, _) = Right $ decodeUtf8 stdout

exceptionToText :: SomeException -> IO (Either String a)
exceptionToText = (pure . Left . displayException)
