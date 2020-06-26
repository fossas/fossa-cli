module Discovery.Walk
  ( -- * Walking the filetree
    walk
  , WalkStep(..)

  , dirName
  , fileName
  ) where

import Prologue

import Path.IO

data WalkStep
  = WalkContinue -- ^ Continue walking subdirectories
  | WalkSkipSome [Path Abs Dir] -- ^ Skip some subdirectories
  | WalkSkipAll -- ^ Skip all subdirectories
  | WalkStop -- ^ Stop walking the filetree entirely
  deriving (Eq, Ord, Show, Generic)

-- | Walk the filetree, rooted at an absolute directory. The passed-in function
-- takes the @currentdir@ @subdirs@ and @files@, and produces a WalkStep
-- describing what to do next.
--
-- You can inspect the names of files and directories with 'dirName' and
-- 'fileName'
walk
  :: MonadIO m
  => (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m WalkStep)
  -> Path Abs Dir
  -> m ()
walk f = walkDir $ \dir subdirs files -> do
  -- normally, subdirs and files are _relative to dir_. We want them to be
  -- relative to the filetree walk
  step <- f dir subdirs files
  case step of
    WalkContinue -> pure (WalkExclude [])
    WalkSkipSome dirs -> pure (WalkExclude dirs)
    WalkSkipAll -> pure (WalkExclude subdirs)
    WalkStop -> pure WalkFinish

dirName :: Path a Dir -> String
dirName = toFilePath . dirname

fileName :: Path a File -> String
fileName = toFilePath . filename
