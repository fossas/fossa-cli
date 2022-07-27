{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.FileTree.IndexFileTree (
    allLeadingPaths,
    zippedPath,
    SomeFileTree(..),
    make
) where

import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, breakOnAll)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import qualified Data.HashTable.IO as H
import Control.Monad (foldM, join)
import Data.Hashable (Hashable, hash)

type HashTable k v = H.BasicHashTable k v

fixedVfsRoot :: Text
fixedVfsRoot = "vfs-root"

data SomeFileTree = SomeFileTree
  { paths :: HashTable SomePath (Maybe Int)
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

empty :: IO (SomeFileTree)
empty = SomeFileTree <$> H.new <*> H.new

doesFileExist :: forall a. Text -> SomeFileTree -> IO (Bool)
doesFileExist t fsTree = f $ toSomePath t
  where
    f (SomeFile file) = isJust <$> H.lookup (paths fsTree) (SomeFile file) 
    f _ = pure False

doesDirExist :: forall a. Text -> SomeFileTree -> IO (Bool)
doesDirExist t fsTree = if (t == fixedVfsRoot)
  then pure True
  else f (toSomePath t)
  where
    f (SomeDir dir) = isJust <$> H.lookup (paths fsTree) (SomeDir dir) 
    f _ = pure False

lookupFileRef :: forall a. Text -> SomeFileTree -> IO (Maybe Int)
lookupFileRef t fsTree = do 
  maybeVal <- f (toSomePath t)
  case maybeVal of
    Nothing -> pure Nothing
    Just m_n -> pure m_n
  where
    f (SomeFile file) = H.lookup (paths fsTree) (SomeFile file) 
    f _ = pure Nothing

lookupDir :: forall a. Text -> SomeFileTree -> IO (Maybe (Set.Set Text))
lookupDir t fsTree =
  case f path of
    Left e -> pure Nothing
    Right x -> do 
      dirr <- x
      case dirr of
        Nothing -> pure $ Just Set.empty
        Just set -> pure $ Just $ Set.map somePathToText set
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

foldrM :: Monad m => (a -> b -> m b) -> m b -> [a] -> m b
foldrM f = foldr ((=<<) . f)

make :: [Text] -> IO (SomeFileTree)
make txt = foldrM (`easyI` Nothing) empty paths
    where
        paths = map toSomePath txt

easyI :: SomePath -> Maybe Int -> SomeFileTree -> IO (SomeFileTree)
easyI a ref st = insert a ref st

insert :: SomePath -> Maybe Int -> SomeFileTree -> IO (SomeFileTree)
insert (SomeDir (SomeDirPath dirpath)) _ tree = do
    foldM insertNode tree $ zippedPath (allLeadingPaths dirpath)
  where
    insertNode :: SomeFileTree -> (Text, Text) -> IO (SomeFileTree)
    insertNode (SomeFileTree paths dirs) (pre, post) = do
        H.insert paths (toSomeDir post) Nothing
        H.insert paths (toSomeDir pre) Nothing
        
        currentValue <- H.lookup dirs (SomeDirPath pre)
        case currentValue of
            Nothing -> do 
              H.insert dirs (SomeDirPath pre) (Set.singleton $ toSomeDir post)
              pure $ SomeFileTree paths dirs
            Just x -> do
                H.insert dirs (SomeDirPath pre) (Set.union x (Set.singleton $ toSomeDir post))
                pure (SomeFileTree paths dirs)
insert (SomeFile someFilePath) someRef tree = do
  let parentDirPath = getParentPath (SomeFile someFilePath)
  let dirsAddedTree = insert (SomeDir parentDirPath) Nothing tree
  flip insertNode parentDirPath =<< dirsAddedTree
  where
    insertNode :: SomeFileTree -> SomeDirPath -> IO (SomeFileTree)
    insertNode (SomeFileTree paths dirs) parentDirPath = do
        H.insert paths (SomeFile someFilePath) someRef
        currentValue <- H.lookup dirs parentDirPath
        case currentValue of
            Nothing -> do
              H.insert dirs parentDirPath (Set.singleton (SomeFile someFilePath)) 
              pure $ SomeFileTree paths dirs
            Just x -> do
                H.insert dirs parentDirPath (Set.union x (Set.singleton (SomeFile someFilePath)))
                pure (SomeFileTree paths dirs)

remove :: SomePath -> SomeFileTree -> IO (SomeFileTree)
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
  pathsWithoutDirs <- fileTreeWithoutChildren
  H.delete (paths pathsWithoutDirs) node

  dirsWithoutSubDirs <- childs
  H.delete dirsWithoutSubDirs (SomeDirPath dirpath)
  pure $ SomeFileTree (paths pathsWithoutDirs) (dirsWithoutSubDirs)
  where
    node = SomeDir (SomeDirPath dirpath)
    
    childrens :: IO (Maybe (Set SomePath))
    childrens = H.lookup (directories tree) (SomeDirPath dirpath) 

    fileTreeWithoutChildren :: IO (SomeFileTree)
    fileTreeWithoutChildren = do 
      desc <- childrens
      case desc of
        Nothing -> pure tree
        Just removedChildren -> foldM (flip remove) tree removedChildren

    childs :: IO (HashTable SomeDirPath (Set SomePath))
    childs = do 
      subTree <- fileTreeWithoutChildren
      
      currentValue <- H.lookup (directories subTree) (getParentPath (SomeDir $ SomeDirPath dirpath))

      case currentValue of
        Nothing -> pure $ directories subTree
        Just x -> do
          H.insert (directories subTree) (getParentPath (SomeDir $ SomeDirPath dirpath)) (Set.delete node x)
          pure $ directories subTree 

allLeadingPaths :: Text -> [Text]
allLeadingPaths path = map fst $ breakOnAll "/" (path <> "/")

zippedPath :: [Text] -> [(Text, Text)]
zippedPath path = zip path (drop 1 path)