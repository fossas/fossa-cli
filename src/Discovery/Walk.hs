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
import Polysemy

type FileWalk r = Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> Sem r (WalkAction Rel)

walkContinue :: Applicative m => m (WalkAction b)
walkContinue = pure $ WalkExclude []

walkSkipAll :: Applicative m => [Path Rel Dir] -> m (WalkAction Rel)
walkSkipAll = pure . WalkExclude . map dirname

walkSkipNamed :: Applicative m => [String] -> [Path Rel Dir] -> m (WalkAction Rel)
walkSkipNamed dirnames = walkSkipAll . filter (\d -> dirName d `elem` dirnames)

walk :: Member (Embed IO) r => FileWalk r -> Path Abs Dir -> Sem r ()
walk f = walkDirRel (\dir subdirs files -> f dir (map (dir </>) subdirs) (map (dir </>) files))

dirName :: Path a Dir -> String
dirName = toFilePath . dirname

fileName :: Path a File -> String
fileName = toFilePath . filename
