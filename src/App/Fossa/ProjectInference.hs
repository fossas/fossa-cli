{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ProjectInference
  ( inferProject,
    mergeOverride,
    InferredProject (..),
  )
where

import App.Types
import Control.Algebra
import Control.Applicative ((<|>))
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Control.Effect.Lift (Lift, sendIO)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (find)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Path
import Path.IO (getTempDir)
import qualified System.FilePath.Posix as FP
import Text.GitConfig.Parser (Section (..), parseConfig)
import Text.Megaparsec (errorBundlePretty)

mergeOverride :: OverrideProject -> InferredProject -> ProjectRevision
mergeOverride OverrideProject {..} InferredProject {..} = ProjectRevision name revision branch
  where
    name = fromMaybe inferredName overrideName
    revision = fromMaybe inferredRevision overrideRevision
    branch = overrideBranch <|> inferredBranch

inferProject :: (Has Logger sig m, Has (Lift IO) sig m) => Path Abs Dir -> m InferredProject
inferProject current = do
  result <- runDiagnostics $ runReadFSIO $ runExecIO (inferGit current <||> inferSVN current)

  case result of
    Right inferred -> pure (resultValue inferred)
    Left failure -> do
      logWarn "Project inference: couldn't find VCS root. Defaulting to directory name."
      logDebug (renderFailureBundle failure)
      inferDefault current

svnCommand :: Command
svnCommand = Command
  { cmdName = "svn"
  , cmdArgs = ["info"]
  , cmdAllowErr = Never
  }

inferSVN :: (Has Exec sig m, Has Diagnostics sig m) => Path b Dir -> m InferredProject
inferSVN dir = do
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
              . dropPrefix "^" $ relUrl

        pure . InferredProject root revision $ if T.null trimmedRelative then Nothing else Just trimmedRelative

  case maybeProject of
    Nothing -> fatal (CommandParseError svnCommand "Invalid output (missing Repository Root or Revision)")
    Just project -> pure project

    where
      toProps :: BL.ByteString -> [(Text, Text)]
      toProps bs = mapMaybe toProp (T.lines (TL.toStrict (decodeUtf8 bs)))
      toProp :: Text -> Maybe (Text, Text)
      toProp propLine =
        case T.splitOn ": " propLine of
          [key, val] -> Just (key, val)
          _ -> Nothing

-- | Infer a default project name from the directory, and a default
-- revision from the current time. Writes `.fossa.revision` to the system
-- temp directory for use by `fossa test`
inferDefault :: Has (Lift IO) sig m => Path b Dir -> m InferredProject
inferDefault dir = sendIO $ do
  let name = FP.dropTrailingPathSeparator (fromRelDir (dirname dir))
  time <- floor <$> getPOSIXTime :: IO Int

  tmp <- getTempDir
  writeFile (fromAbsDir tmp FP.</> ".fossa.revision") (show time)

  pure (InferredProject (T.pack name) (T.pack (show time)) Nothing)

-- like Text.stripPrefix, but with a non-Maybe result (defaults to the original text)
dropPrefix :: Text -> Text -> Text
dropPrefix pre txt = fromMaybe txt (T.stripPrefix pre txt)

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
  ) => Path Abs Dir -> m InferredProject
inferGit dir = do
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
  )
  => Path Abs Dir -> m Text
parseGitProjectName dir = do
  let relConfig = [relfile|config|]

  exists <- doesFileExist (dir </> relConfig)

  unless exists (fatal MissingGitConfig)

  contents <- readContentsText (dir </> relConfig)

  case parseConfig contents of
    Left err -> fatal (GitConfigParse (T.pack (errorBundlePretty err)))
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
  )
  => Path Abs Dir -> m (Maybe Text, Text) -- branch, revision
parseGitProjectRevision dir = do
  let relHead = [relfile|HEAD|]

  headExists <- doesFileExist (dir </> relHead)

  unless headExists (fatal MissingGitHead)

  headText <- readContentsText (dir </> relHead)

  if "ref: " `T.isPrefixOf` headText
    then do
      let rawPath = removeNewlines . dropPrefix "ref: " $ headText

      case parseRelFile (T.unpack rawPath) of
        Nothing -> fatal (InvalidBranchName rawPath)
        Just path -> do
          branchExists <- doesFileExist (dir </> path)

          unless branchExists (fatal (MissingBranch rawPath))

          revision <- removeNewlines <$> readContentsText (dir </> path)
          let branch = dropPrefix "refs/heads/" rawPath
          pure (Just branch, revision)
    else pure (Nothing, T.strip headText)

removeNewlines :: Text -> Text
removeNewlines = T.replace "\r" "" . T.replace "\n" ""

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
    InvalidRemote -> "Missing 'origin' git remote"
    GitConfigParse err -> "An error occurred when parsing the git config: " <> pretty err
    MissingGitConfig -> "Missing .git/config file"
    MissingGitHead -> "Missing .git/HEAD file"
    InvalidBranchName branch -> "Invalid branch name: " <> pretty branch
    MissingBranch branch -> "Missing ref file for current branch: " <> pretty branch
    MissingGitDir -> "Could not find .git directory in the current or any parent directory"

data InferredProject = InferredProject
  { inferredName :: Text,
    inferredRevision :: Text,
    inferredBranch :: Maybe Text
  }
  deriving (Eq, Ord, Show)
