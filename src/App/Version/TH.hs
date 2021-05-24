{-# LANGUAGE TemplateHaskell #-}

module App.Version.TH
  ( getCurrentTag,
  )
where

import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Effect.Diagnostics (Diagnostics, fromEitherShow)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.String.Conversion (decodeUtf8)
import Data.Versions (errorBundlePretty, semver)
import Effect.Exec
  ( AllowErr (Always),
    Command (..),
    Exec,
    Has,
    exec,
    runExecIO,
  )
import GitHash (giHash, tGitInfoCwd)
import Instances.TH.Lift ()
import Language.Haskell.TH (TExpQ)
import Language.Haskell.TH.Syntax (reportWarning, runIO)
import Path (Dir, Rel, mkRelDir)

gitTagPointCommand :: Text -> Command
gitTagPointCommand commit =
  Command
    { cmdName = "git",
      cmdArgs = ["tag", "--points-at", commit],
      cmdAllowErr = Always
    }

getCurrentTag :: TExpQ (Maybe Text)
getCurrentTag = do
  let commitHash = giHash $$(tGitInfoCwd)
  result <- runIO . runDiagnostics . runExecIO . getTags $ T.pack commitHash

  case result of
    Left err -> reportWarning (show err) >> [||Nothing||]
    Right tags -> filterTags tags

getTags :: (Has Exec sig m, Has Diagnostics sig m) => Text -> m [Text]
getTags hash = do
  -- FIXME: boy it would be nice to not shell out during compilation.  So much that could go wrong.
  result <- exec $(mkRelDir ".") $ gitTagPointCommand hash
  bsl <- fromEitherShow result

  pure . map T.strip . T.lines . decodeUtf8 $ BSL.toStrict bsl

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
filterTags xs = reportWarning (T.unpack multiTagMesg) >> [||Nothing||]
  where
    multiTagMesg = header <> T.intercalate ", " xs
    header = "Multiple tags defined at current commit: "

validateSingleTag :: Text -> TExpQ (Maybe Text)
validateSingleTag tag = do
  let normalized = fromMaybe tag $ T.stripPrefix "v" tag

  case semver normalized of
    Left err -> reportWarning (errorBundlePretty err) >> [||Nothing||]
    Right _ -> [||Just normalized||]
