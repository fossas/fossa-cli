module Discovery.Walk
  ( walk

  , dirName
  , fileName

  , walkContinue
  , walkSkipAll
  , walkSkipNamed
  ) where

import Prologue

import Path.IO

type FileWalk m = Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m (WalkAction Rel)

walkContinue :: Applicative m => m (WalkAction b)
walkContinue = pure $ WalkExclude []

walkSkipAll :: Applicative m => [Path Rel Dir] -> m (WalkAction Rel)
walkSkipAll = pure . WalkExclude . map dirname

-- TODO: this is brittle: strings must end with a `/`
walkSkipNamed :: Applicative m => [String] -> [Path Rel Dir] -> m (WalkAction Rel)
walkSkipNamed dirnames = walkSkipAll . filter (\d -> dirName d `elem` dirnames)

walk :: MonadIO m => FileWalk m -> Path Abs Dir -> m ()
walk f = walkDirRel (\dir subdirs files -> f dir (map (dir </>) subdirs) (map (dir </>) files))

dirName :: Path a Dir -> String
dirName = toFilePath . dirname

fileName :: Path a File -> String
fileName = toFilePath . filename
