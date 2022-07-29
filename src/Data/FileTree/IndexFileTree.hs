{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.FileTree.IndexFileTree (
  SomeFileTree (..),

  -- * getters
  doesFileExist,
  doesDirExist,
  lookupFileRef,
  lookupDir,
  resolveSymLinkRef,

  -- * modifiers
  insert,
  remove,
  empty,

  -- * utility functions
  toSomePath,
  allLeadingPaths,
  zippedPath,
) where

import Control.DeepSeq (NFData)
import Data.Foldable (Foldable (foldr'), foldl')
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as H
import Data.Hashable (Hashable, hash)
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, breakOnAll)
import Data.Text qualified as Text
import GHC.Generics (Generic)

fixedVfsRoot :: Text
fixedVfsRoot = "vfs-root"

-- | Simple FileSystem Representation.
--
-- From investigation, this representation/structure can be substantially further
-- optimized. By using impure dictionary implementations which use IORef, this
-- filetree is ~2.3x faster and has less peak memory consumption, and heap memory
-- usage. However, it is not implemented now to avoid use of impurity/IO.
--
-- In future, if we need to optimize operation around FileTree, it is recommended that,
-- we use HashTable, or vector-dictionary module.
--
-- Refer to POC code:
-- * https://github.com/fossas/fossa-cli/pull/1005/commits/6f7ed2c2e946b469285cdb2ec97572f1b5fbf389
-- * https://github.com/fossas/fossa-cli/pull/1005/commits/f2892c48ff896002b119426ae71eb0c521a2ad44
data SomeFileTree a = SomeFileTree
  { paths :: HashMap SomePath (Maybe a)
  , directories :: HashMap SomeDirPath (Set SomePath)
  }
  deriving (Show, Generic, NFData)

empty :: SomeFileTree a
empty = SomeFileTree mempty mempty

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
    then SomeDir $ SomeDirPath (withoutSuffixSlash candidate)
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
doesFileExist :: forall a. Text -> SomeFileTree a -> Bool
doesFileExist t fsTree = f $ toSomePath t
  where
    f :: SomePath -> Bool
    f (SomeFile file) = isJust $ H.lookup (SomeFile file) (paths fsTree)
    f _ = False

-- | Returns True if Dir Exists, Otherwise False.
doesDirExist :: forall a. Text -> SomeFileTree a -> Bool
doesDirExist t fsTree = (t == fixedVfsRoot) || f (toSomePath t)
  where
    f :: SomePath -> Bool
    f (SomeDir dir) = isJust $ H.lookup (SomeDir dir) (paths fsTree)
    f _ = False

-- | Returns reference associated with the filepath (if any).
lookupFileRef :: forall a. Text -> SomeFileTree a -> Maybe a
lookupFileRef t fsTree = fromMaybe Nothing (f $ toSomePath t)
  where
    f :: SomePath -> Maybe (Maybe a)
    f (SomeFile file) = H.lookup (SomeFile file) (paths fsTree)
    f _ = Nothing

-- | Returns List of immediate paths that are in the directory (if any)
-- If directory does not exist, returns Nothing;
-- If directory is empty, returns Empty Set;
lookupDir :: forall a. Text -> SomeFileTree a -> Maybe (Set Text)
lookupDir t fsTree =
  case f path of
    Left _ -> Nothing
    Right subDirs -> do
      case subDirs of
        Nothing -> Just Set.empty
        Just setPaths -> Just $ Set.map somePathToText setPaths
  where
    path :: SomePath
    path = if t == fixedVfsRoot then SomeDir $ SomeDirPath fixedVfsRoot else toSomePath t

    f :: SomePath -> Either (Maybe a) (Maybe (Set SomePath))
    f (SomeDir dir) = Right $ H.lookup dir (directories fsTree)
    f _ = Left Nothing

-- | Removes suffix '/' if any.
withoutSuffixSlash :: Text -> Text
withoutSuffixSlash c = fromMaybe c $ Text.stripSuffix "/" c

-- | Makes Directory.
toSomeDir :: Text -> SomePath
toSomeDir a = SomeDir (SomeDirPath a)

-- | Gets parent directory of path.
getParentPath :: SomePath -> SomeDirPath
getParentPath (SomeFile (SomeFilePath filepath)) = SomeDirPath (withoutSuffixSlash $ fst (Text.breakOnEnd "/" filepath))
getParentPath (SomeDir (SomeDirPath filepath)) = SomeDirPath (withoutSuffixSlash $ fst (Text.breakOnEnd "/" filepath))

-- | Inserts SomePath into FileTree with Reference.
--
-- If the path already exists in the FileTree, it updates to provided reference.
-- For path which are directories, No reference is recorded.
--
-- >> insert "a/hello.txt" (Just 123)
-- >> insert "a/archive/"
insert :: forall a. SomePath -> Maybe a -> SomeFileTree a -> SomeFileTree a
insert (SomeDir (SomeDirPath dirPath)) _ tree = foldl' insertDirNode tree $ pathPairs dirPath
  where
    -- Ensure Predecessor and Successor Directory
    -- exists in the Paths (if not already)
    insertedPaths :: SomeFileTree a -> (Text, Text) -> HashMap SomePath (Maybe a)
    insertedPaths (SomeFileTree paths _) (pre, post) = H.insert (toSomeDir post) Nothing $ H.insert (toSomeDir pre) Nothing paths

    insertDirNode :: SomeFileTree a -> (Text, Text) -> SomeFileTree a
    insertDirNode (SomeFileTree paths dirs) (pre, post) =
      -- If Predecessor Directory exists in our Directory Listing,
      -- Add Successor Directory into set of Successors. If Predecessor
      -- directory does not exist, Create a listing and add Successor
      -- directory into Set.
      case H.lookup (SomeDirPath pre) dirs of
        Nothing -> do
          SomeFileTree
            (insertedPaths (SomeFileTree paths dirs) (pre, post))
            (H.insert (SomeDirPath pre) (Set.singleton $ toSomeDir post) dirs)
        Just setOfSuccessors -> do
          SomeFileTree
            (insertedPaths (SomeFileTree paths dirs) (pre, post))
            (H.insert (SomeDirPath pre) (Set.union setOfSuccessors . Set.singleton $ toSomeDir post) dirs)
insert (SomeFile someFilePath) someRef tree =
  -- Ensure all leading directories exist in fileTree.
  -- E.g.
  --
  --  A/B/hello.txt
  --
  -- Ensure, path A and A/B exist in fileTree, and
  -- A's directory listing includes A/B.
  -- let parentDirPath = getParentPath (SomeFile someFilePath)
  -- We don't store ref for Dirs
  insertFileNode parentDirPath $ insert (SomeDir parentDirPath) Nothing tree
  where
    parentDirPath :: SomeDirPath
    parentDirPath = getParentPath (SomeFile someFilePath)

    -- Add filepath in paths listing
    updatedPath :: HashMap SomePath (Maybe a) -> HashMap SomePath (Maybe a)
    updatedPath = H.insert (SomeFile someFilePath) someRef

    insertFileNode :: SomeDirPath -> SomeFileTree a -> SomeFileTree a
    insertFileNode parentDir (SomeFileTree paths dirs) =
      -- Ensure File exists in directory listing
      -- of it's parent directory.
      case (H.lookup parentDir dirs) of
        Nothing -> do
          SomeFileTree
            (updatedPath paths)
            (H.insert parentDir (Set.singleton $ SomeFile someFilePath) dirs)
        Just subDirListing -> do
          SomeFileTree
            (updatedPath paths)
            (H.insert parentDir (Set.union subDirListing . Set.singleton $ SomeFile someFilePath) dirs)

-- | Removes FilePath from File tree.
--
-- If the FilePath is a directory, it's removes all successor filepaths
-- from file tree (i.e. recursively removes).
remove :: forall a. SomePath -> SomeFileTree a -> SomeFileTree a
remove (SomeFile filepath) (SomeFileTree paths dirs) =
  case (H.lookup parentPath dirs) of
    Nothing -> SomeFileTree withoutPath dirs
    Just subDirListing -> do
      SomeFileTree withoutPath (H.insert parentPath (Set.filter (/= SomeFile filepath) subDirListing) dirs)
  where
    -- Remove from Path Listing
    withoutPath :: HashMap SomePath (Maybe a)
    withoutPath = H.delete (SomeFile filepath) paths

    parentPath :: SomeDirPath
    parentPath = getParentPath $ SomeFile filepath
remove (SomeDir dirPath) tree = SomeFileTree (H.delete dir $ paths fsWithoutSubPaths) (directories fsWithoutSubPaths)
  where
    dir :: SomePath
    dir = SomeDir dirPath

    -- Gets all directories listing
    subPathListing :: (Maybe (Set SomePath))
    subPathListing = H.lookup dirPath (directories tree)

    -- Recursively removed sub paths from directory
    subPathsRemoved :: SomeFileTree a
    subPathsRemoved = case subPathListing of
      Nothing -> tree
      Just subPathsToRemove -> foldr' remove tree subPathsToRemove

    -- Removed from Parent's sub dir listing.
    rmdFromParentSubDirListing :: HashMap SomeDirPath (Set SomePath)
    rmdFromParentSubDirListing = case H.lookup (getParentPath $ SomeDir dirPath) (directories subPathsRemoved) of
      Nothing -> directories subPathsRemoved
      Just subDirsListing -> H.insert (getParentPath $ SomeDir dirPath) (Set.delete dir subDirsListing) (directories subPathsRemoved)

    fsWithoutSubPaths :: (SomeFileTree a)
    fsWithoutSubPaths = SomeFileTree (paths subPathsRemoved) $ H.delete dirPath (rmdFromParentSubDirListing)

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

-- | Resolves Filepath's SymLink references in related to candidate working filepath.
--
-- >> resolveSymLinkRef "a/b/c" "./d" = "a/b/d"
-- >> resolveSymLinkRef "a/b/c" "../d" = "a/d"
-- >> resolveSymLinkRef "a/b/c" "../../d" = "d"
-- >> resolveSymLinkRef "a/b/c" "/d" = "d"
--
-- If it runs of of parent directory of candidate working path, it presumes
-- target is located at root.
--
-- >> resolveSymLinkRef "a/b/c" "../../../../../d" = "d"
--
-- It presumes candidate working filepath is not a directory. Directory SymLinks
-- resolution is not supported.
--
-- It does not handle, '../' or './' between non-relative filepaths, e.g.
-- /a/b/c/../d/../e, ./a/b/../e, etc.
-- -
resolveSymLinkRef :: Text -> Text -> Text
resolveSymLinkRef cwd targetPath =
  case (Text.isPrefixOf "./" targetPath, Text.isPrefixOf "../" targetPath, Text.isPrefixOf "/" targetPath) of
    (True, _, _) -> uncurry resolveSymLinkRef $ skipToParentDir cwd targetPath
    (_, True, _) -> uncurry resolveSymLinkRef $ skipToGrandParentDir cwd targetPath
    (_, _, True) -> fromMaybe targetPath $ Text.stripPrefix "/" targetPath
    (False, False, False) -> cwd <> targetPath
  where
    parentDir :: Text
    parentDir = "./"

    grandParentDir :: Text
    grandParentDir = "../"

    withoutPrefix :: Text -> Text -> Text
    withoutPrefix p t = fromMaybe t $ Text.stripPrefix p t

    skipToGrandParentDir :: Text -> Text -> (Text, Text)
    skipToGrandParentDir currentCwd target =
      if Text.isPrefixOf grandParentDir target
        then
          ( fst $ Text.breakOnEnd "/" (withoutSuffixSlash $ fst $ Text.breakOnEnd "/" cwd)
          , withoutPrefix grandParentDir target
          )
        else (currentCwd, target)

    skipToParentDir :: Text -> Text -> (Text, Text)
    skipToParentDir currentCwd target =
      if Text.isPrefixOf parentDir target
        then
          ( fst $ Text.breakOnEnd "/" currentCwd
          , withoutPrefix parentDir target
          )
        else (currentCwd, target)
