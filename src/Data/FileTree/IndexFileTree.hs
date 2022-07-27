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

data SomeFileTree a = SomeFileTree
  { paths :: HashTable SomePath (Maybe a)
  , directories :: HashTable SomeDirPath (Set.Set SomePath)
  }
  deriving (Show, Generic)

data SomePath
  = SomeDir !SomeDirPath
  | SomeFile !SomeFilePath
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
  if Text.isSuffixOf "/" candidate
    then SomeDir $ SomeDirPath (withoutSlash candidate)
    else SomeFile $ SomeFilePath candidate
  where
    candidate :: Text
    candidate = fixedVfsRoot <> "/" <> c

somePathToText :: SomePath -> Text
somePathToText (SomeDir (SomeDirPath dir)) = Text.replace (fixedVfsRoot <> "/") "" dir <> "/"
somePathToText (SomeFile (SomeFilePath file)) = Text.replace (fixedVfsRoot <> "/") "" file

empty :: IO (SomeFileTree a)
empty = SomeFileTree <$> H.new <*> H.new

doesFileExist :: forall a. Text -> SomeFileTree a -> IO (Bool)
doesFileExist t fsTree = f $ toSomePath t
  where
    f (SomeFile file) = isJust <$> H.lookup (paths fsTree) (SomeFile file)
    f _ = pure False

doesDirExist :: forall a. Text -> SomeFileTree a -> IO (Bool)
doesDirExist t fsTree = if (t == fixedVfsRoot) then pure True else f (toSomePath t)
  where
    f (SomeDir dir) = isJust <$> H.lookup (paths fsTree) (SomeDir dir)
    f _ = pure False

lookupFileRef :: forall a. Text -> SomeFileTree a -> IO (Maybe a)
lookupFileRef t fsTree = do
  maybeVal <- f (toSomePath t)
  case maybeVal of
    Nothing -> pure Nothing
    Just m_n -> pure m_n
  where
    f (SomeFile file) = H.lookup (paths fsTree) (SomeFile file)
    f _ = pure Nothing

lookupDir :: forall a. Text -> SomeFileTree a -> IO (Maybe (Set.Set Text))
lookupDir t fsTree =
  case f path of
    Left _ -> pure Nothing
    Right subDirsIO -> do
      subDirs <- subDirsIO
      case subDirs of
        Nothing -> pure $ Just Set.empty
        Just fp -> pure $ Just $ Set.map somePathToText fp
  where
    path = if t == fixedVfsRoot then SomeDir $ SomeDirPath fixedVfsRoot else toSomePath t
    f (SomeDir dir) = Right $ H.lookup (directories fsTree) dir
    f _ = Left Nothing

withoutSlash :: Text -> Text
withoutSlash c = fromMaybe c $ Text.stripSuffix "/" c

toSomeDir :: Text -> SomePath
toSomeDir a = SomeDir (SomeDirPath a)

getParentPath :: SomePath -> SomeDirPath
getParentPath (SomeFile (SomeFilePath filepath)) = SomeDirPath (withoutSlash $ fst (Text.breakOnEnd "/" filepath))
getParentPath (SomeDir (SomeDirPath filepath)) = SomeDirPath (withoutSlash $ fst (Text.breakOnEnd "/" filepath))

-- | Strict foldL
-- from: https://stackoverflow.com/a/8919106/19573624
foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = pure z
foldM' f z (x : xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

insert :: forall a. SomePath -> Maybe a -> SomeFileTree a -> IO (SomeFileTree a)
insert (SomeDir (SomeDirPath dirpath)) _ tree = do
  foldM' insertNode tree $ zippedPath (allLeadingPaths dirpath)
  where
    insertNode :: SomeFileTree a -> (Text, Text) -> IO (SomeFileTree a)
    insertNode (SomeFileTree paths dirs) (pre, post) = do
      -- Add parent and child dir in paths
      H.insert paths (toSomeDir post) Nothing
      H.insert paths (toSomeDir pre) Nothing

      -- If parent dir exists, add child in it's directories listing
      subDirs <- H.lookup dirs (SomeDirPath pre)
      case subDirs of
        Nothing -> do
          H.insert dirs (SomeDirPath pre) (Set.singleton $ toSomeDir post)
          pure $ SomeFileTree paths dirs
        Just x -> do
          H.insert dirs (SomeDirPath pre) (Set.union x (Set.singleton $ toSomeDir post))
          pure (SomeFileTree paths dirs)
insert (SomeFile someFilePath) someRef tree = do
  -- Ensure parent path exists
  let parentDirPath = getParentPath (SomeFile someFilePath)
  let fs = insert (SomeDir parentDirPath) Nothing tree

  -- Add file path
  insertNode parentDirPath =<< fs
  where
    insertNode :: SomeDirPath -> SomeFileTree a -> IO (SomeFileTree a)
    insertNode parentDirPath (SomeFileTree paths dirs) = do
      -- Add file path in paths listing with ref
      H.insert paths (SomeFile someFilePath) someRef

      -- Add file in it's directories listing
      subDirs <- H.lookup dirs parentDirPath
      case subDirs of
        Nothing -> do
          H.insert dirs parentDirPath (Set.singleton (SomeFile someFilePath))
          pure $ SomeFileTree paths dirs
        Just subDirListing -> do
          H.insert dirs parentDirPath (Set.union subDirListing $ Set.singleton (SomeFile someFilePath))
          pure (SomeFileTree paths dirs)

remove :: forall a. SomePath -> SomeFileTree a -> IO (SomeFileTree a)
remove (SomeFile (SomeFilePath filepath)) (SomeFileTree paths dirs) = do
  H.delete paths node
  currentValue <- H.lookup dirs parent
  case currentValue of
    Nothing -> do
      pure $ SomeFileTree paths dirs
    Just x -> do
      H.insert dirs parent (Set.filter (/= node) x)
      pure (SomeFileTree paths dirs)
  where
    node = SomeFile (SomeFilePath filepath)
    parent = getParentPath node
remove (SomeDir (SomeDirPath dirpath)) tree = do
  -- Remove Dir from Paths
  pathsWithoutDirs <- fsWithoutSubDirsAndFiles
  H.delete (paths pathsWithoutDirs) node

  -- Remove sub dirs and files recursively
  dirsWithoutSubDirs <- childs
  H.delete dirsWithoutSubDirs (SomeDirPath dirpath)

  pure $ SomeFileTree (paths pathsWithoutDirs) (dirsWithoutSubDirs)
  where
    node = SomeDir (SomeDirPath dirpath)

    subDirsAndFiles :: IO (Maybe (Set SomePath))
    subDirsAndFiles = H.lookup (directories tree) (SomeDirPath dirpath)

    fsWithoutSubDirsAndFiles :: IO (SomeFileTree a)
    fsWithoutSubDirsAndFiles = do
      fs <- subDirsAndFiles
      case fs of
        Nothing -> pure tree
        Just removedChildren -> foldM (flip remove) tree removedChildren

    childs :: IO (HashTable SomeDirPath (Set SomePath))
    childs = do
      fs <- fsWithoutSubDirsAndFiles
      currentValue <- H.lookup (directories fs) (getParentPath (SomeDir $ SomeDirPath dirpath))
      case currentValue of
        Nothing -> pure $ directories fs
        Just x -> do
          H.insert (directories fs) (getParentPath (SomeDir $ SomeDirPath dirpath)) (Set.delete node x)
          pure $ directories fs

-- | Generates all path leading up to given path
-- >> allLeadingPaths "A/B/C" = ["A", "A/B", "A/B/C"]
allLeadingPaths :: Text -> [Text]
allLeadingPaths path = map fst $ breakOnAll "/" (path <> "/")

-- | Zips path together in parent child pairs
-- zippedPath ["A", "A/B", "A/B/C"] = [("A", "A/B"), ("A/B", "A/B/C")]
zippedPath :: [Text] -> [(Text, Text)]
zippedPath path = zip path (drop 1 path)
