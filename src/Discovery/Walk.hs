module Discovery.Walk (
  -- * Walking the filetree
  walk,
  walk',
  walkWithFilters',
  WalkStep (..),

  -- * Helpers
  fileName,
  findFileNamed,
  findFirstMatchingFile,
  findFilesMatchingGlob,
) where

import Control.Carrier.Writer.Church
import Control.Effect.Diagnostics
import Control.Effect.Reader (Reader, ask)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Foldable (find)
import Data.Functor (void)
import Data.Glob qualified as Glob
import Data.List ((\\))
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Discovery.Filters (AllFilters, pathAllowed)
import Effect.ReadFS
import Path
import Debug.Trace (traceM)

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
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m WalkStep) ->
  Path Abs Dir ->
  m ()
walk f = walkDir $ \dir subdirs files -> do
  -- Check that the path matches the filters
  step <- f dir subdirs files
  case step of
    WalkContinue -> pure $ WalkExclude []
    WalkSkipSome dirs ->
      -- we normalize the passed in [Text] as relative directories for more reliable comparisons
      let parsedDirs = mapMaybe (parseRelDir . toString) dirs
       in pure . WalkExclude . filter ((`elem` parsedDirs) . dirname) $ subdirs
    WalkSkipAll -> pure $ WalkExclude subdirs
    WalkStop -> pure WalkFinish

pathFilterIntercept ::
  ( Applicative m
  , Monoid o
  ) =>
  AllFilters ->
  Path Abs Dir ->
  Path Abs Dir ->
  m (o, WalkStep) ->
  m (o, WalkStep)
pathFilterIntercept filters base path act = do
  -- We know that the two have the same base, but if that invariant is broken,
  -- we just allow the path during discovery.  It's better than crashing.
  case stripProperPrefix base path of
    Nothing -> act
    Just relative ->
      if pathAllowed filters relative
        then act
        else pure (mempty, WalkSkipAll)

-- |Like @walk@, but collects the output of @f@ in a monoid.
walk' ::
  forall o sig m.
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Monoid o
  ) =>
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (o, WalkStep)) ->
  Path Abs Dir ->
  m o
walk' f base = do
  foo <- runWriter (curry pure) $ walk mangled base
  pure (fst foo)
  where
    mangled :: Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> WriterC o m WalkStep
    mangled dir subdirs files = do
      (res, step) <- lift $ f dir subdirs files
      tell res
      pure step

-- | Like @walk'@, but ignores paths that don't match the provided filters.
walkWithFilters' ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  , Monoid o
  ) =>
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (o, WalkStep)) ->
  Path Abs Dir ->
  m o
walkWithFilters' f root = do
  filters <- ask
  let f' dir subdirs files = pathFilterIntercept filters root dir $ f dir subdirs files
  walk' f' root

fileName :: Path a File -> String
fileName = toFilePath . filename

findFileNamed :: String -> [Path a File] -> Maybe (Path a File)
findFileNamed name = find (\f -> fileName f == name)

-- | Find the first file that matches any of the provided names.
findFirstMatchingFile :: [String] -> [Path a File] -> Maybe (Path a File)
findFirstMatchingFile names = find (\f -> fileName f `Set.member` names')
  where
    names' = Set.fromList names

findFilesMatchingGlob :: Glob.Glob a -> [Path a File] -> [Path a File]
findFilesMatchingGlob g = filter (`Glob.matches` g)

-------------- Stolen from path-io; adapted to our own ReadFS effect

walkDir ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  -- | Handler (@dir -> subdirs -> files -> 'WalkAction'@)
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (WalkAction Abs)) ->
  -- | Directory where traversal begins
  Path Abs Dir ->
  m ()
walkDir handler topdir =
  context "Walking the filetree" $
    void $
      -- makeAbsolute topdir >>= walkAvoidLoop Set.empty
      walkAvoidLoop Set.empty topdir
  where
    walkAvoidLoop traversed curdir = do
      mRes <- checkLoop traversed curdir
      case mRes of
        Nothing -> pure $ Just ()
        Just traversed' -> walktree traversed' curdir
    walktree traversed curdir = do
      (subdirs, files) <- listDir curdir
      action <- handler curdir subdirs files
      case action of
        WalkFinish -> pure Nothing
        WalkExclude xdirs ->
          case subdirs \\ xdirs of
            [] -> pure $ Just ()
            ds ->
              runMaybeT $
                mapM_
                  (MaybeT . walkAvoidLoop traversed)
                  ds
    checkLoop :: (Has ReadFS sig m, Has Diagnostics sig m) => Set.Set DirID -> Path Abs Dir -> m (Maybe (Set.Set DirID))
    checkLoop traversed dir = do
      traceM $ "Checking for loop in " ++ (show dir)
      maybeIdentifier <- recover $ getIdentifier dir
      traceM $ "maybeIdentifier: " ++ show maybeIdentifier
      case maybeIdentifier of
        Nothing -> pure Nothing
        Just identifier ->
          pure $
            if Set.member identifier traversed
              then Nothing
              else Just (Set.insert identifier traversed)

data WalkAction b
  = -- | Finish the entire walk altogether
    WalkFinish
  | -- | List of sub-directories to exclude from
    -- descending
    WalkExclude [Path b Dir]
  deriving (Eq, Show)
