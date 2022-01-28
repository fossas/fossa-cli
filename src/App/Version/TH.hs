{-# LANGUAGE TemplateHaskell #-}

module App.Version.TH (
  getCurrentTag,
) where

import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Control.Effect.Diagnostics (Diagnostics, fromEitherShow)
import Control.Effect.Lift (Lift, sendIO)
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
import Effect.Logger (ignoreLogger)
import GitHash (giHash, tGitInfoCwd)
import Instances.TH.Lift ()
import Language.Haskell.TH (TExpQ)
import Language.Haskell.TH.Syntax (reportWarning, runIO)
import Path.IO (getCurrentDir)

gitTagPointCommand :: Text -> Command
gitTagPointCommand commit =
  Command
    { cmdName = "git"
    , cmdArgs = ["tag", "--points-at", commit]
    , cmdAllowErr = Always
    }

getCurrentTag :: TExpQ (Maybe Text)
getCurrentTag = do
  let commitHash = giHash $$(tGitInfoCwd)
  result <- runIO . ignoreLogger . runStack . runDiagnostics . runExecIO . getTags $ toText commitHash

  case result of
    Success _ tags -> filterTags tags
    err -> reportWarning (show err) >> [||Nothing||]

getTags :: (Has (Lift IO) sig m, Has Exec sig m, Has Diagnostics sig m) => Text -> m [Text]
getTags hash = do
  dir <- sendIO getCurrentDir
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
filterTags :: [Text] -> TExpQ (Maybe Text)
filterTags [] = [||Nothing||]
filterTags [x] = validateSingleTag x
filterTags xs = reportWarning (toString multiTagMesg) >> [||Nothing||]
  where
    multiTagMesg = header <> Text.intercalate ", " xs
    header = "Multiple tags defined at current commit: "

validateSingleTag :: Text -> TExpQ (Maybe Text)
validateSingleTag tag = do
  let normalized = fromMaybe tag $ Text.stripPrefix "v" tag

  case semver normalized of
    Left err -> reportWarning (errorBundlePretty err) >> [||Nothing||]
    Right _ -> [||Just normalized||]
