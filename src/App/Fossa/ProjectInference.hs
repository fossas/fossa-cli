{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.ProjectInference (
  inferProjectFromVCS,
  inferProjectCached,
  inferProjectDefault,
  inferProjectDefaultFromFile,
  saveRevision,
  mergeOverride,
  readCachedRevision,
  InferredProject (..),

  -- * for testing
  linesWithoutCR,
) where

import App.Types
import Control.Algebra
import Control.Applicative ((<|>))
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Control.Effect.Lift (Lift, sendIO)
import Control.Monad (unless)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (find)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Conversion (decodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Effect.Exec
import Effect.ReadFS
import Errata (Errata (..))
import Path
import Path.IO (getTempDir)
import System.FilePath.Posix qualified as FP
import Text.GitConfig.Parser (Section (..), parseConfig)
import Text.Megaparsec (errorBundlePretty)

revisionFileName :: Path Rel File
revisionFileName = $(mkRelFile ".fossa.revision")

mergeOverride :: OverrideProject -> InferredProject -> ProjectRevision
mergeOverride OverrideProject{..} InferredProject{..} = ProjectRevision name revision branch
  where
    name = fromMaybe inferredName overrideName
    revision = fromMaybe inferredRevision overrideRevision
    branch = overrideBranch <|> inferredBranch

-- TODO: pass ReadFS and Exec constraints upward
inferProjectFromVCS :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m InferredProject
inferProjectFromVCS current = inferGit current <||> inferSVN current

-- | Similar to 'inferProjectDefault', but uses a saved revision
inferProjectCached :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m) => Path b Dir -> m InferredProject
inferProjectCached dir = do
  project <- inferProjectDefault dir
  rev <- readCachedRevision
  pure project{inferredRevision = rev}

-- | Infer a default project name from the directory, and a default revision from the current time.
inferProjectDefault :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path b Dir -> m InferredProject
inferProjectDefault dir = context "Inferring project from directory name / timestamp" . sendIO $ do
  let name = FP.dropTrailingPathSeparator (fromRelDir (dirname dir))
  time <- iso8601Show <$> getCurrentTime

  let stamp = Text.takeWhile (/= '.') $ toText time -- trim milliseconds off, format is yyyy-mm-ddThh:mm:ss[.sss]
  pure (InferredProject (toText name) (stamp <> "Z") Nothing)

-- | Infer a default project name from a filename, and a default revision from the current time.
inferProjectDefaultFromFile :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path b File -> m InferredProject
inferProjectDefaultFromFile file = context "Inferring project from filename / timestamp" . sendIO $ do
  let name = FP.dropTrailingPathSeparator (fromRelFile (filename file))
  time <- iso8601Show <$> getCurrentTime

  let stamp = Text.takeWhile (/= '.') $ toText time -- trim milliseconds off, format is yyyy-mm-ddThh:mm:ss[.sss]
  pure (InferredProject (toText name) (stamp <> "Z") Nothing)

svnCommand :: Command
svnCommand =
  Command
    { cmdName = "svn"
    , cmdArgs = ["info"]
    , cmdAllowErr = Never
    }

inferSVN :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m InferredProject
inferSVN dir = context "Inferring project from SVN" $ do
  output <- execThrow dir svnCommand
  let props = toProps output

  let maybeProject = do
        root <- lookup "Repository Root" props
        revision <- lookup "Revision" props
        url <- lookup "URL" props
        relUrl <- lookup "Relative URL" props

        let rootRelativeToUrl = dropPrefix url root

        -- we need to trim off: the caret, the root (as relative to the url), and one of "/branches/" or "/"
        let trimmedRelative =
              dropPrefix "branches/"
                . dropPrefix "/"
                . dropPrefix rootRelativeToUrl
                . dropPrefix "^"
                $ relUrl

        pure . InferredProject root revision $ if Text.null trimmedRelative then Nothing else Just trimmedRelative

  case maybeProject of
    Nothing -> fatal (CommandParseError svnCommand "Invalid output (missing Repository Root or Revision)")
    Just project -> pure project
  where
    toProps :: BL.ByteString -> [(Text, Text)]
    toProps bs = mapMaybe toProp (linesWithoutCR (decodeUtf8 bs))

    toProp :: Text -> Maybe (Text, Text)
    toProp propLine =
      case Text.splitOn ": " propLine of
        [key, val] -> Just (key, val)
        _ -> Nothing

-- Removes Windows `\r` from suffix if any
-- We do this since: Text.lines does not remove \r.
linesWithoutCR :: Text -> [Text]
linesWithoutCR = Text.lines . Text.replace "\r\n" "\n"

saveRevision :: Has (Lift IO) sig m => ProjectRevision -> m ()
saveRevision project = do
  tmp <- sendIO getTempDir
  sendIO $ TIO.writeFile (fromAbsFile $ tmp </> revisionFileName) (projectRevision project)

readCachedRevision :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m) => m Text
readCachedRevision = do
  tmp <- sendIO getTempDir
  readContentsText $ tmp </> revisionFileName

-- like Text.stripPrefix, but with a non-Maybe result (defaults to the original text)
dropPrefix :: Text -> Text -> Text
dropPrefix pre txt = fromMaybe txt (Text.stripPrefix pre txt)

findGitDir :: Has ReadFS sig m => Path Abs Dir -> m (Maybe (Path Abs Dir))
findGitDir dir = do
  let relGit = [reldir|.git|]

  exists <- doesDirExist (dir </> relGit)
  if exists
    then pure (Just (dir </> relGit))
    else do
      let parentDir = parent dir
      if parentDir /= dir
        then findGitDir parentDir
        else pure Nothing

inferGit ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m InferredProject
inferGit dir = context "Inferring project from git" $ do
  foundGitDir <- findGitDir dir

  case foundGitDir of
    Nothing -> fatal MissingGitDir
    Just gitDir -> do
      name <- parseGitProjectName gitDir
      (branch, revision) <- parseGitProjectRevision gitDir
      pure (InferredProject name revision branch)

parseGitProjectName ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m Text
parseGitProjectName dir = do
  let relConfig = [relfile|config|]

  exists <- doesFileExist (dir </> relConfig)

  unless exists (fatal MissingGitConfig)

  contents <- readContentsText (dir </> relConfig)

  case parseConfig contents of
    Left err -> fatal (GitConfigParse (toText (errorBundlePretty err)))
    Right config -> do
      let maybeSection = find isOrigin config
      case maybeSection of
        Nothing -> fatal InvalidRemote
        Just (Section _ properties) ->
          case HM.lookup "url" properties of
            Just url -> pure url
            Nothing -> fatal InvalidRemote
  where
    isOrigin :: Section -> Bool
    isOrigin (Section ["remote", "origin"] _) = True
    isOrigin _ = False

parseGitProjectRevision ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m (Maybe Text, Text) -- branch, revision
parseGitProjectRevision dir = do
  let relHead = [relfile|HEAD|]

  headExists <- doesFileExist (dir </> relHead)

  unless headExists (fatal MissingGitHead)

  headText <- readContentsText (dir </> relHead)

  if "ref: " `Text.isPrefixOf` headText
    then do
      let rawPath = removeNewlines . dropPrefix "ref: " $ headText

      case parseRelFile (toString rawPath) of
        Nothing -> fatal (InvalidBranchName rawPath)
        Just path -> do
          branchExists <- doesFileExist (dir </> path)

          unless branchExists (fatal (MissingBranch rawPath))

          revision <- removeNewlines <$> readContentsText (dir </> path)
          let branch = dropPrefix "refs/heads/" rawPath
          pure (Just branch, revision)
    else pure (Nothing, Text.strip headText)

removeNewlines :: Text -> Text
removeNewlines = Text.replace "\r" "" . Text.replace "\n" ""

data InferenceError
  = InvalidRemote
  | GitConfigParse Text
  | MissingGitConfig
  | MissingGitHead
  | InvalidBranchName Text
  | MissingBranch Text
  | MissingGitDir
  deriving (Eq, Ord, Show)

instance ToDiagnostic InferenceError where
  renderDiagnostic = \case
    InvalidRemote -> do
      let header = "Missing 'origin' git remote"
      Errata (Just header) [] Nothing
    GitConfigParse err -> do
      let header = "An error occurred when parsing the git config: " <> err
      Errata (Just header) [] Nothing
    MissingGitConfig -> do
      let header = "Missing .git/config file"
      Errata (Just header) [] Nothing
    MissingGitHead -> do
      let header = "Missing .git/HEAD file"
      Errata (Just header) [] Nothing
    InvalidBranchName branch -> do
      let header = "Invalid branch name: " <> branch
      Errata (Just header) [] Nothing
    MissingBranch branch -> do
      let header = "Missing ref file for current branch: " <> branch
      Errata (Just header) [] Nothing
    MissingGitDir -> do
      let header = "Could not find .git directory in the current or any parent directory"
      Errata (Just header) [] Nothing

data InferredProject = InferredProject
  { inferredName :: Text
  , inferredRevision :: Text
  , inferredBranch :: Maybe Text
  }
  deriving (Eq, Ord, Show)
