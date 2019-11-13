module App.Scan.ProjectInference
  ( inferProject
  , InferredProject(..)
  ) where

import Prologue

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Path.IO (getTempDir)
import           Polysemy
import           Polysemy.Error
import qualified System.FilePath.Posix as FP
import           Text.GitConfig.Parser (Section(..), parseConfig)
import           Text.Megaparsec (errorBundlePretty)

import Diagnostics
import Effect.Logger
import Effect.ReadFS

inferProject :: Members '[Embed IO, Logger] r => Path Abs Dir -> Sem r InferredProject
inferProject current = do
  -- gitInferred :: Either (CLIErr (Either GitInferenceError InferredProject))
  gitInferred <- inferGit current & readFSToIO & runError @GitInferenceError & runError @CLIErr

  case gitInferred of
    Left err -> do
      logWarn ("Project inference: unexpected error. Defaulting to directory name. error: " <> viaShow err)
      inferDefault current

    Right (Left err) -> do
      logWarn ("Project inference: couldn't find VCS root. Defaulting to directory name. error: " <> viaShow err)
      inferDefault current

    Right (Right project) -> pure project

-- | Infer a default project name from the directory, and a default
-- revision from the current time. Writes `.fossa.revision` to the system
-- temp directory for use by `fossa test`
inferDefault :: Member (Embed IO) r => Path b Dir -> Sem r InferredProject
inferDefault dir = do
  let name = FP.dropTrailingPathSeparator (fromRelDir (dirname dir))
  time <- embed (floor <$> getPOSIXTime :: IO Int)

  tmp <- getTempDir
  embed (writeFile ((fromAbsDir tmp) FP.</> ".fossa.revision") (show time))

  pure (InferredProject (T.pack name) (T.pack (show time)))


findGitDir :: Member ReadFS r => Path Abs Dir -> Sem r (Maybe (Path Abs Dir))
findGitDir dir = do
  let Just relGit = parseRelDir ".git"

  exists <- doesDirExist (dir </> relGit)
  if exists
    then pure (Just (dir </> relGit))
    else do
      let parentDir = parent dir
      if parentDir /= dir
        then findGitDir parentDir
        else pure Nothing

inferGit :: Members '[ReadFS, Error GitInferenceError] r => Path Abs Dir -> Sem r InferredProject
inferGit currentDir = do
  foundGitDir <- findGitDir currentDir

  case foundGitDir of
    Nothing -> throw MissingGitDir
    Just dir -> do
      name     <- parseGitProjectName dir
      revision <- parseGitProjectRevision dir
      pure (InferredProject name revision)

parseGitProjectName :: Members '[ReadFS, Error GitInferenceError] r => Path Abs Dir -> Sem r Text
parseGitProjectName dir = do
  let Just relConfig = parseRelFile "config"

  exists <- doesFileExist (dir </> relConfig)

  when (not exists) (throw MissingGitConfig)

  contents <- readContentsText (dir </> relConfig)

  case parseConfig contents of
    Left err -> throw (GitConfigParse (T.pack (errorBundlePretty err)))

    Right config -> do
      let maybeSection = find (isOrigin) config
      case maybeSection of
        Nothing -> throw InvalidRemote
        Just (Section _ properties) -> do
          case HM.lookup "url" properties of
            Just url -> pure url
            Nothing -> throw InvalidRemote

  where
  isOrigin :: Section -> Bool
  isOrigin (Section ["remote", "origin"] _) = True
  isOrigin _ = False

parseGitProjectRevision :: Members '[ReadFS, Error GitInferenceError] r => Path Abs Dir -> Sem r Text
parseGitProjectRevision dir = do
  let Just relHead = parseRelFile "HEAD"

  headExists <- doesFileExist (dir </> relHead)

  when (not headExists) (throw MissingGitHead)

  contents <- removeNewlines . T.drop 5 <$> readContentsText (dir </> relHead)

  case parseRelFile (T.unpack contents) of
    Just path -> do
      branchExists <- doesFileExist (dir </> path)

      when (not branchExists) (throw (MissingBranch contents))

      removeNewlines <$> readContentsText (dir </> path)

    Nothing -> throw (InvalidBranchName contents)

removeNewlines :: Text -> Text
removeNewlines = T.replace "\r" "" . T.replace "\n" ""

data GitInferenceError =
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
