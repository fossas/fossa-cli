{-# language QuasiQuotes #-}

module App.Fossa.ProjectInference
  ( inferProject
  , InferredProject(..)
  ) where

import Prologue

import Control.Algebra
import Control.Carrier.Error.Either
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Path.IO (getTempDir)
import qualified System.FilePath.Posix as FP
import Text.GitConfig.Parser (Section(..), parseConfig)
import Text.Megaparsec (errorBundlePretty)

import Effect.Exec
import Effect.Logger
import Effect.ReadFS

inferProject :: (Has Logger sig m, MonadIO m) => Path Abs Dir -> m InferredProject
inferProject current = do
  gitInferred <- inferGit current & runReadFSIO & runError @ReadFSErr & runError @InferenceError
  svnInferred <- inferSVN current & runExecIO & runError @ExecErr & runError @InferenceError

  case (gitInferred, svnInferred) of
    -- we found a git project
    (Right (Right project), _) -> pure project
    -- we found an svn project
    (_, Right (Right project))         -> pure project

    _ -> do
      logWarn "Project inference: couldn't find VCS root. Defaulting to directory name."
      inferDefault current

svnCommand :: Command
svnCommand = Command
  { cmdNames = ["svn"]
  , cmdBaseArgs = ["info"]
  , cmdAllowErr = Never
  }

inferSVN :: (Has Exec sig m, Has (Error ExecErr) sig m) => Path b Dir -> m InferredProject
inferSVN dir = do
  output <- execThrow dir svnCommand []
  let props = toProps output
  case (,) <$> lookup "Repository Root" props <*> lookup "Revision" props of
    Just (name, rev) -> pure (InferredProject name rev)
    Nothing -> throwError (CommandParseError "svn" "Invalid output (missing Repository Root or Revision)")

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
inferDefault :: MonadIO m => Path b Dir -> m InferredProject
inferDefault dir = liftIO $ do
  let name = FP.dropTrailingPathSeparator (fromRelDir (dirname dir))
  time <- floor <$> getPOSIXTime :: IO Int

  tmp <- getTempDir
  writeFile (fromAbsDir tmp FP.</> ".fossa.revision") (show time)

  pure (InferredProject (T.pack name) (T.pack (show time)))


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
  , Has (Error ReadFSErr) sig m
  , Has (Error InferenceError) sig m
  ) => Path Abs Dir -> m InferredProject
inferGit dir = do
  foundGitDir <- findGitDir dir

  case foundGitDir of
    Nothing -> throwError MissingGitDir
    Just gitDir -> do
      name     <- parseGitProjectName gitDir
      revision <- parseGitProjectRevision gitDir
      pure (InferredProject name revision)

parseGitProjectName ::
  ( Has ReadFS sig m
  , Has (Error ReadFSErr) sig m
  , Has (Error InferenceError) sig m
  )
  => Path Abs Dir -> m Text
parseGitProjectName dir = do
  let relConfig = [relfile|config|]

  exists <- doesFileExist (dir </> relConfig)

  unless exists (throwError MissingGitConfig)

  contents <- readContentsText (dir </> relConfig)

  case parseConfig contents of
    Left err -> throwError (GitConfigParse (T.pack (errorBundlePretty err)))

    Right config -> do
      let maybeSection = find isOrigin config
      case maybeSection of
        Nothing -> throwError InvalidRemote
        Just (Section _ properties) ->
          case HM.lookup "url" properties of
            Just url -> pure url
            Nothing -> throwError InvalidRemote

  where
  isOrigin :: Section -> Bool
  isOrigin (Section ["remote", "origin"] _) = True
  isOrigin _ = False

parseGitProjectRevision ::
  ( Has ReadFS sig m
  , Has (Error ReadFSErr) sig m
  , Has (Error InferenceError) sig m
  )
  => Path Abs Dir -> m Text
parseGitProjectRevision dir = do
  let relHead = [relfile|HEAD|]

  headExists <- doesFileExist (dir </> relHead)

  unless headExists (throwError MissingGitHead)

  contents <- removeNewlines . T.drop 5 <$> readContentsText (dir </> relHead)

  case parseRelFile (T.unpack contents) of
    Just path -> do
      branchExists <- doesFileExist (dir </> path)

      unless branchExists (throwError (MissingBranch contents))

      removeNewlines <$> readContentsText (dir </> path)

    Nothing -> throwError (InvalidBranchName contents)

removeNewlines :: Text -> Text
removeNewlines = T.replace "\r" "" . T.replace "\n" ""

data InferenceError =
    InvalidRemote
  | GitConfigParse Text
  | MissingGitConfig
  | MissingGitHead
  | InvalidBranchName Text
  | MissingBranch Text
  | MissingGitDir
  deriving (Eq, Ord, Show, Generic, Typeable)


data InferredProject = InferredProject
  { inferredName     :: Text
  , inferredRevision :: Text
  } deriving (Eq, Ord, Show, Generic)
