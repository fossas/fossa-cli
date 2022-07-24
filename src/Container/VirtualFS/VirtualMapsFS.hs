{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Container.VirtualFS.VirtualMapsFS (
  insert,
  remove,
  lookupFileRef,
  lookupDir,
  empty,
  doesFileExist,
  doesDirExist,
  SomeFileTree,
  SomePath,
  SomeDirPath (..),
  SomeFilePath (..),
  toSomePath,
) where

import Control.DeepSeq (NFData)
import Control.Monad qualified
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Strict qualified as MapStrict
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, breakOnAll)
import Data.Text qualified as Text
import GHC.Generics (Generic)

fixedVfsRoot :: Text
fixedVfsRoot = "vfs-root"

-- | Very Simple (and not performant) File-tree.
--
-- TODO:
--
--  * This structure is storage expensive and stores redundant component information multiple times.
--  * This structure uses Map, which in haskell are O(ln n) unlike O(1), which increases cost per operation.
--
-- We should be using component trie like structure, in which leaf nodes are file.
--
-- e.g.
--
--  etc/              => [Component 0]
--  etc/apk/          => [Component 0, Component 1]
--  etc/apk/installed => [Component 0, Component 1, Component 2] * leaf
--  etc/apk/log       => [Component 0, Component 1, Component 3] * leaf
data SomeFileTree a = SomeFileTree
  { paths :: Map SomePath (Maybe a)
  , directories :: Map (SomeDirPath) (Set.Set SomePath)
  }
  deriving (Show, Eq, Ord, Generic, NFData)

data SomePath
  = SomeDir !SomeDirPath
  | SomeFile !SomeFilePath
  deriving (Eq, Ord, Generic, NFData)

instance Show SomePath where
  show (SomeDir dir) = show dir
  show (SomeFile file) = show file

newtype SomeDirPath = SomeDirPath Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData) -- We explicitly use DeriveAnyClass strategy to avoid deriving-defaults warning.

newtype SomeFilePath = SomeFilePath Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData) -- We explicitly use DeriveAnyClass strategy to avoid deriving-defaults warning.

toSomePath :: Text -> SomePath
toSomePath c =
  if Text.isSuffixOf "/" candidate
    then SomeDir $ SomeDirPath (withoutSlash candidate)
    else SomeFile $ SomeFilePath candidate
  where
    candidate :: Text
    candidate = fixedVfsRoot <> "/" <> c

somePathToText :: SomePath -> Text
somePathToText (SomeDir (SomeDirPath dir)) = (Text.replace (fixedVfsRoot <> "/") "" dir) <> "/"
somePathToText (SomeFile (SomeFilePath file)) = Text.replace (fixedVfsRoot <> "/") "" file

empty :: SomeFileTree a
empty = SomeFileTree mempty mempty

doesFileExist :: forall a. Text -> SomeFileTree a -> Bool
doesFileExist t fsTree = f $ toSomePath t
  where
    f (SomeFile file) = Map.member (SomeFile file) (paths fsTree)
    f (_) = False

doesDirExist :: forall a. Text -> SomeFileTree a -> Bool
doesDirExist t fsTree = (t == fixedVfsRoot) || f (toSomePath t)
  where
    f (SomeDir dir) = Map.member (SomeDir dir) (paths fsTree)
    f (_) = False

lookupFileRef :: forall a. Text -> SomeFileTree a -> Maybe a
lookupFileRef t fsTree = Control.Monad.join $ f (toSomePath t)
  where
    f (SomeFile file) = Map.lookup (SomeFile file) (paths fsTree)
    f (_) = Nothing

lookupDir :: forall a. Text -> SomeFileTree a -> Maybe (Set.Set Text)
lookupDir t fsTree = case f path of
  Left e -> e
  Right (Just x) -> Just $ Set.map somePathToText x
  Right (Nothing) -> Just Set.empty
  where
    path = if t == fixedVfsRoot then (SomeDir $ SomeDirPath fixedVfsRoot) else toSomePath t
    f (SomeDir dir) = Right $ Map.lookup dir (directories fsTree)
    f (_) = Left Nothing

withoutSlash :: Text -> Text
withoutSlash c = fromMaybe c $ Text.stripSuffix "/" c

toSomeDir :: Text -> SomePath
toSomeDir a = SomeDir (SomeDirPath a)

getParentPath :: SomePath -> SomeDirPath
getParentPath (SomeFile (SomeFilePath filepath)) = SomeDirPath (withoutSlash $ fst (Text.breakOnEnd "/" filepath))
getParentPath (SomeDir (SomeDirPath filepath)) = SomeDirPath (withoutSlash $ fst (Text.breakOnEnd "/" filepath))

insert :: forall a. (Show a) => SomePath -> Maybe a -> SomeFileTree a -> SomeFileTree a
insert (SomeDir (SomeDirPath dirpath)) _ tree = foldl (insertNode) tree $ zippedPath (allLeadingPaths dirpath)
  where
    insertNode :: SomeFileTree a -> (Text, Text) -> SomeFileTree a
    insertNode itree (pre, post) =
      SomeFileTree
        { paths = Map.insert (toSomeDir pre) Nothing $ Map.insert (toSomeDir post) Nothing (paths itree)
        , directories = MapStrict.insertWith (<>) (SomeDirPath pre) (Set.singleton (toSomeDir post)) (directories itree)
        }
insert (SomeFile someFilePath) someRef tree = do
  let parentDirPath = getParentPath (SomeFile someFilePath)
  let dirsAddedTree = insert (SomeDir parentDirPath) Nothing tree
  insertNode dirsAddedTree parentDirPath
  where
    insertNode :: SomeFileTree a -> SomeDirPath -> SomeFileTree a
    insertNode itree parentDirPath =
      SomeFileTree
        { paths = Map.insert (SomeFile someFilePath) someRef (paths itree)
        , directories = MapStrict.insertWith (<>) parentDirPath (Set.singleton ((SomeFile someFilePath))) (directories itree)
        }

remove :: forall a. Show a => SomePath -> SomeFileTree a -> SomeFileTree a
remove (SomeFile (SomeFilePath filepath)) tree =
  SomeFileTree
    { paths = Map.filterWithKey (\k _ -> k /= node) (paths tree)
    , directories = Map.adjust (Set.delete node) (parent) (directories tree)
    }
  where
    node = (SomeFile (SomeFilePath filepath))
    parent = getParentPath node
remove (SomeDir (SomeDirPath dirpath)) tree =
  SomeFileTree
    { paths = Map.filterWithKey (\k _ -> k /= node) (paths fileTreeWithoutChildren)
    , directories = Map.filterWithKey (\k _ -> k /= (SomeDirPath dirpath)) childs
    }
  where
    node = (SomeDir (SomeDirPath dirpath))
    childrens :: Maybe (Set SomePath)
    childrens = Map.lookup (SomeDirPath dirpath) (directories tree)

    fileTreeWithoutChildren :: SomeFileTree a
    fileTreeWithoutChildren = case childrens of
      Nothing -> tree
      Just removedChildren -> foldl (flip remove) tree removedChildren

    childs :: Map SomeDirPath (Set SomePath)
    childs = Map.adjust (Set.delete node) (getParentPath (SomeDir $ SomeDirPath dirpath)) (directories fileTreeWithoutChildren)

allLeadingPaths :: Text -> [Text]
allLeadingPaths path = map fst $ breakOnAll "/" (path <> "/")

zippedPath :: [Text] -> [(Text, Text)]
zippedPath path = zip path (drop 1 path)
