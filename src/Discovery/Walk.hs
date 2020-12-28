{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Discovery.Walk
  ( -- * Walking the filetree
    walk,
    walk',
    WalkStep (..),

    -- * Helpers
    fileName,
    findFileNamed,
  )
where

import Control.Carrier.Writer.Church
import Control.Monad.Trans
import Control.Carrier.Lift ( runM, LiftC )
import Data.Foldable (find)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Path
import Path.IO

data WalkStep
  = -- | Continue walking subdirectories
    WalkContinue
  | -- | Skip some subdirectories
    WalkSkipSome [Text]
  | -- | Skip all subdirectories
    WalkSkipAll
  | -- | Stop walking the filetree entirely
    WalkStop
  deriving (Eq, Ord, Show)

-- | Walk the filetree, rooted at an absolute directory. The passed-in function
-- takes the @currentdir@ @subdirs@ and @files@, and produces a WalkStep
-- describing what to do next.
--
-- You can inspect the names of files and directories with 'dirName' and
-- 'fileName'
walk ::
  MonadIO m =>
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m WalkStep) ->
  Path Abs Dir ->
  m ()
walk f = walkDir $ \dir subdirs files -> do
  -- normally, subdirs and files are _relative to dir_. We want them to be
  -- relative to the filetree walk
  step <- f dir subdirs files
  case step of
    WalkContinue -> pure $ WalkExclude []
    WalkSkipSome dirs ->
      -- we normalize the passed in [Text] as relative directories for more reliable comparisons
      let parsedDirs = mapMaybe (parseRelDir . T.unpack) dirs
       in pure . WalkExclude . filter ((`elem` parsedDirs) . dirname) $ subdirs
    WalkSkipAll -> pure $ WalkExclude subdirs
    WalkStop -> pure WalkFinish

walk' ::
  forall m o.
  (MonadIO m, Monoid o) =>
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (o, WalkStep)) ->
  Path Abs Dir ->
  m o
walk' f base = runM $ do
  foo <- runWriter (\w a -> pure (w, a)) $ walk mangled base
  pure (fst foo)
    where
      mangled :: Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> WriterC o (LiftC m) WalkStep
      mangled dir subdirs files = do
        (res, step) <- lift $ lift $ f dir subdirs files
        tell res
        pure step

fileName :: Path a File -> String
fileName = toFilePath . filename

findFileNamed :: String -> [Path a File] -> Maybe (Path a File)
findFileNamed name files = find (\f -> fileName f == name) files
