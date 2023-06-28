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
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Data.String.Conversion (decodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Versions (errorBundlePretty, semver)
import Diag.Result (Result (Success))
import Language.Haskell.TH.Syntax
    ( runIO, Quasi(qAddDependentFile), reportWarning, runIO )
import Language.Haskell.TH
    ( bindCode, Code, Q, bindCode_, joinCode )
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
import System.Process.Typed (readProcess)

gitTagPointCommand :: Text -> Command
gitTagPointCommand commit =
  Command
    { cmdName = "git"
    , cmdArgs = ["tag", "--points-at", commit]
    , cmdAllowErr = Always
    }

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

themisVersionQ :: Code Q String
themisVersionQ =
  (do qAddDependentFile "vendor-bins/themis-cli"
      runIO (readProcess "vendor-bins/themis-cli --version"))
  `bindCode` spliceVersion
  where spliceVersion (_exitCode, stdOut, _stdErr) = do
          let versionStr = decodeUtf8 stdOut
          [||versionStr||]

