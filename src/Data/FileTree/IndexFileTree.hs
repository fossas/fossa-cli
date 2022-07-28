{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.FileTree.IndexFileTree (
  allLeadingPaths,
  zippedPath,
  SomeFileTree (..),
  doesFileExist,
  doesDirExist,
  lookupFileRef,
  lookupDir,
  insert,
  remove,
  empty,
  toSomePath,
) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable, hash)
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, breakOnAll)
import Data.Text qualified as Text
import GHC.Generics (Generic)

type HashTable k v = H.BasicHashTable k v

-- FIX ME
fixedVfsRoot :: Text
fixedVfsRoot = "vfs-root"

-- | Simple FileSystem Representation.
data SomeFileTree a = SomeFileTree
  { paths :: HashTable SomePath (Maybe a)
  , directories :: HashTable SomeDirPath (Set.Set SomePath)
  }
  deriving (Show, Generic)

empty :: IO (SomeFileTree a)
empty = SomeFileTree <$> H.new <*> H.new

data SomePath
  = SomeDir SomeDirPath
  | SomeFile SomeFilePath
  deriving (Eq, Ord, Generic, NFData)

instance Show SomePath where
  show (SomeDir dir) = show dir
  show (SomeFile file) = show file

instance Hashable SomePath where
  hash (SomeDir (SomeDirPath dir)) = hash dir
  hash (SomeFile (SomeFilePath file)) = hash file

newtype SomeDirPath = SomeDirPath Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData) -- explicitly use DeriveAnyClass strategy to avoid deriving-defaults warning.

newtype SomeFilePath = SomeFilePath Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData) -- explicitly use DeriveAnyClass strategy to avoid deriving-defaults warning.

instance Hashable SomeDirPath where
  hash (SomeDirPath dir) = hash dir

instance Hashable SomeFilePath where
  hash (SomeFilePath file) = hash file

toSomePath :: Text -> SomePath
toSomePath c =
  if Text.isSuffixOf "/" candidate -- In POSIX, path with / suffix are directories
    then SomeDir $ SomeDirPath (withoutSlash candidate)
    else SomeFile $ SomeFilePath candidate
  where
    candidate :: Text
    candidate = fixedVfsRoot <> "/" <> c

-- | Converts SomePath to Text (in POSIX format)
-- Directories will have suffix / added
somePathToText :: SomePath -> Text
somePathToText (SomeDir (SomeDirPath dir)) = Text.replace (fixedVfsRoot <> "/") "" dir <> "/"
somePathToText (SomeFile (SomeFilePath file)) = Text.replace (fixedVfsRoot <> "/") "" file

-- | Returns True if File Exists, Otherwise False.
doesFileExist :: forall a. Text -> SomeFileTree a -> IO (Bool)
doesFileExist t fsTree = f $ toSomePath t
  where
    f :: SomePath -> IO Bool
    f (SomeFile file) = isJust <$> H.lookup (paths fsTree) (SomeFile file)
    f _ = pure False

-- | Returns True if Dir Exists, Otherwise False.
doesDirExist :: forall a. Text -> SomeFileTree a -> IO (Bool)
doesDirExist t fsTree = if (t == fixedVfsRoot) then pure True else f (toSomePath t)
  where
    f :: SomePath -> IO Bool
    f (SomeDir dir) = isJust <$> H.lookup (paths fsTree) (SomeDir dir)
    f _ = pure False

-- | Returns reference associated with the filepath (if any).
lookupFileRef :: forall a. Text -> SomeFileTree a -> IO (Maybe a)
lookupFileRef t fsTree = do
  refVal <- f (toSomePath t)
  case refVal of
    Nothing -> pure Nothing
    Just rv -> pure rv
  where
    f :: SomePath -> IO (Maybe (Maybe a))
    f (SomeFile file) = H.lookup (paths fsTree) (SomeFile file)
    f _ = pure Nothing

-- | Returns List of immediate paths that are in the directory (if any)
-- If directory does not exist, returns Nothing;
-- If directory is empty, returns Empty Set;
lookupDir :: forall a. Text -> SomeFileTree a -> IO (Maybe (Set.Set Text))
lookupDir t fsTree =
  case f path of
    Left _ -> pure Nothing
    Right act -> do
      subDirs <- act
      case subDirs of
        Nothing -> pure $ Just Set.empty
        Just setPaths -> pure $ Just $ Set.map somePathToText setPaths
  where
    path :: SomePath
    path = if t == fixedVfsRoot then SomeDir $ SomeDirPath fixedVfsRoot else toSomePath t

    f :: SomePath -> Either (Maybe a) (IO (Maybe (Set SomePath)))
    f (SomeDir dir) = Right $ H.lookup (directories fsTree) dir
    f _ = Left Nothing

-- | Removes suffix '/' if any.
withoutSlash :: Text -> Text
withoutSlash c = fromMaybe c $ Text.stripSuffix "/" c

-- | Makes Directory.
toSomeDir :: Text -> SomePath
toSomeDir a = SomeDir (SomeDirPath a)

-- | Gets parent directory of path.
getParentPath :: SomePath -> SomeDirPath
getParentPath (SomeFile (SomeFilePath filepath)) = SomeDirPath (withoutSlash $ fst (Text.breakOnEnd "/" filepath))
getParentPath (SomeDir (SomeDirPath filepath)) = SomeDirPath (withoutSlash $ fst (Text.breakOnEnd "/" filepath))

-- | Inserts SomePath into FileTree with Reference.
--
-- If the path already exists in the FileTree, it updates to provided reference.
-- For path which are directories, No reference is recorded.
--
-- >> insert "a/hello.txt" (Just 123)
-- >> insert "a/archive/"
insert :: forall a. SomePath -> Maybe a -> SomeFileTree a -> IO (SomeFileTree a)
insert (SomeDir (SomeDirPath dirPath)) _ tree = foldM' insertDirNode tree $ pathPairs dirPath
  where
    insertDirNode :: SomeFileTree a -> (Text, Text) -> IO (SomeFileTree a)
    insertDirNode (SomeFileTree paths dirs) (pre, post) = do
      -- Ensure Predecessor and Successor Directory
      -- exists in the Paths (if not already)
      H.insert paths (toSomeDir post) Nothing
      H.insert paths (toSomeDir pre) Nothing

      -- If Predecessor Directory exists in our Directory Listing,
      -- Add Successor Directory into set of Successors. If Predecessor
      -- directory does not exist, Create a listing and add Successor
      -- directory into Set.
      subDirListing <- H.lookup dirs (SomeDirPath pre)
      case subDirListing of
        Nothing -> do
          H.insert dirs (SomeDirPath pre) (Set.singleton $ toSomeDir post)
          pure $ SomeFileTree paths dirs
        Just setOfSuccessors -> do
          H.insert dirs (SomeDirPath pre) (Set.union setOfSuccessors . Set.singleton $ toSomeDir post)
          pure $ SomeFileTree paths dirs
insert (SomeFile someFilePath) someRef tree = do
  -- Ensure all leading directories exist in fileTree.
  -- E.g.
  --
  --  A/B/hello.txt
  --
  -- Ensure, path A and A/B exist in fileTree, and
  -- A's directory listing includes A/B.
  let parentDirPath = getParentPath (SomeFile someFilePath)
  let fs = insert (SomeDir parentDirPath) Nothing tree -- We don't store ref for Dirs
  insertFileNode parentDirPath =<< fs
  where
    insertFileNode :: SomeDirPath -> SomeFileTree a -> IO (SomeFileTree a)
    insertFileNode parentDirPath (SomeFileTree paths dirs) = do
      -- Add filepath in paths listing
      H.insert paths (SomeFile someFilePath) someRef

      -- Ensure File exists in directory listing
      -- of it's parent directory.
      subDirs <- H.lookup dirs parentDirPath
      case subDirs of
        Nothing -> do
          H.insert dirs parentDirPath (Set.singleton $ SomeFile someFilePath)
          pure $ SomeFileTree paths dirs
        Just subDirListing -> do
          H.insert dirs parentDirPath (Set.union subDirListing . Set.singleton $ SomeFile someFilePath)
          pure $ SomeFileTree paths dirs

-- | Removes FilePath from File tree.
--
-- If the FilePath is a directory, it's removes all successor filepaths
-- from file tree (i.e. recursively removes).
remove :: forall a. SomePath -> SomeFileTree a -> IO (SomeFileTree a)
remove (SomeFile filepath) (SomeFileTree paths dirs) = do
  -- Remove from Path Listing
  H.delete paths $ SomeFile filepath

  -- Remove from It's parents directory listing.
  subDirs <- H.lookup dirs parentPath
  case subDirs of
    Nothing -> pure $ SomeFileTree paths dirs
    Just subDirListing -> do
      H.insert dirs parentPath (Set.filter (/= SomeFile filepath) subDirListing)
      pure $ SomeFileTree paths dirs
  where
    parentPath :: SomeDirPath
    parentPath = getParentPath $ SomeFile filepath
remove (SomeDir dirPath) tree = do
  -- Recursively remove all successor filepaths
  fsWoSubPaths <- fsWithoutSubPaths

  -- Remove directory filepath from paths listing
  H.delete (paths fsWoSubPaths) dir

  pure fsWoSubPaths
  where
    dir :: SomePath
    dir = SomeDir dirPath

    -- Gets all directories listing
    subPathListing :: IO (Maybe (Set SomePath))
    subPathListing = H.lookup (directories tree) dirPath

    fsWithoutSubPaths :: IO (SomeFileTree a)
    fsWithoutSubPaths = do
      subPaths <- subPathListing

      -- Get FS with all sub paths removed.
      fs <- case subPaths of
        Nothing -> pure tree
        Just subPathsToRemove -> foldM (flip remove) tree subPathsToRemove

      -- Get parent directory of current path
      subDirs <- H.lookup (directories fs) (getParentPath $ SomeDir dirPath)

      -- Modify Parent directories directory listing to ensure
      -- path to be removed is not present
      dirsRemoved <- case subDirs of
        Nothing -> pure $ directories fs
        Just subDirsListing -> do
          H.insert (directories fs) (getParentPath $ SomeDir dirPath) (Set.delete dir subDirsListing)
          pure $ directories fs

      -- Remove dirpath from directory listing
      H.delete (dirsRemoved) dirPath
      pure $ SomeFileTree (paths fs) dirsRemoved

-- | Generates paths pairs of predecessor, and successors from a path.
-- >> pathPairs "A/B/hello.txt" = [("A", "A/B"), ("A/B", "A/B/hello.txt")]
pathPairs :: Text -> [(Text, Text)]
pathPairs = zippedPath . allLeadingPaths

-- | Generates all path leading up to given path
-- >> allLeadingPaths "A/B/C" = ["A", "A/B", "A/B/C"]
allLeadingPaths :: Text -> [Text]
allLeadingPaths path = map fst $ breakOnAll "/" (path <> "/")

-- | Zips path together in parent child pairs
-- zippedPath ["A", "A/B", "A/B/C"] = [("A", "A/B"), ("A/B", "A/B/C")]
zippedPath :: [Text] -> [(Text, Text)]
zippedPath path = zip path (drop 1 path)

-- | Strict foldL
-- from: https://stackoverflow.com/a/8919106/19573624
foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = pure z
foldM' f z (x : xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs
