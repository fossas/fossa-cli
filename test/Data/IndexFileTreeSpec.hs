module Data.IndexFileTreeSpec (spec) where

import Data.FileTree.IndexFileTree (
  SomeFileTree,
  doesDirExist,
  doesFileExist,
  empty,
  insert,
  lookupDir,
  lookupFileRef,
  remove,
  toSomePath,
 )

import Data.Set qualified as Set
import Data.Text (Text)
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )

refVal1 :: Maybe Int
refVal1 = Just 1

refVal2 :: Maybe Int
refVal2 = Just 2

mkTree :: [(Text, Maybe Int)] -> IO (SomeFileTree Int)
mkTree = foldrM (\(p, ref) tree -> insert (toSomePath p) ref tree) empty
  where
    -- monadic foldr
    foldrM :: Monad m => (a -> b -> m b) -> m b -> [a] -> m b
    foldrM f = foldr ((=<<) . f)

sampleFileTree :: IO (SomeFileTree Int)
sampleFileTree =
  mkTree
    [ ("root.txt", refVal1)
    , ("archive/index.txt", refVal2)
    , ("archive/jan/1.txt", refVal1)
    , ("archive/feb/1.txt", refVal1)
    , ("archive/feb/extra/info.txt", refVal1)
    ]

shouldReturn :: (Show a, Eq a) => IO a -> a -> IO ()
shouldReturn lhs rhs = do
  lhsActed <- lhs
  lhsActed `shouldBe` rhs

insertSpec :: Spec
insertSpec =
  describe "insert" $ do
    let insertPath = insert . toSomePath

    it "should insert root level filepath node" $ do
      let fs = insertPath "a.txt" refVal1 =<< empty
      (doesFileExist "a.txt" =<< fs) `shouldReturn` True

    it "should insert nested filepath node" $ do
      let fs =
            insertPath "tmp/a.txt" refVal1
              =<< empty

      (doesFileExist "tmp/a.txt" =<< fs) `shouldReturn` True

    it "should insert nested filepath node, when parent filepath node already exists" $ do
      let fs =
            insertPath "tmp/b.txt" refVal2
              =<< insertPath "tmp/a.txt" refVal1
              =<< empty

      (doesFileExist "tmp/a.txt" =<< fs) `shouldReturn` True
      (doesFileExist "tmp/b.txt" =<< fs) `shouldReturn` True

    it "should insert (multi-level) filepath nested node" $ do
      let fs = insertPath "tmp/archive/a.txt" refVal1 =<< empty
      (doesFileExist "tmp/archive/a.txt" =<< fs) `shouldReturn` True

    it "should insert (multi-level) filepath nested node, when parent filepath node already exists" $ do
      let fs =
            insertPath "tmp/archive/b.txt" refVal2
              =<< insertPath "tmp/archive/a.txt" refVal1
              =<< empty

      (doesFileExist "tmp/archive/a.txt" =<< fs) `shouldReturn` True
      (doesFileExist "tmp/archive/b.txt" =<< fs) `shouldReturn` True

    it "should update existing root level filepath node, if filepath node already exists" $ do
      let fs = insertPath "a.txt" refVal2 =<< (insertPath "a.txt" refVal1 =<< empty)
      (lookupFileRef "a.txt" =<< fs) `shouldReturn` refVal2

    it "should update existing nested filepath node, if filepath node already exists" $ do
      let fs = insertPath "tmp/a.txt" refVal2 =<< insertPath "tmp/a.txt" refVal1 =<< empty
      (lookupFileRef "tmp/a.txt" =<< fs) `shouldReturn` refVal2

    it "should update existing (multi-level) nested filepath node, if filepath node already exists" $ do
      let fs = insertPath "tmp/archive/a.txt" refVal2 =<< insertPath "tmp/archive/a.txt" refVal1 =<< empty
      (lookupFileRef "tmp/archive/a.txt" =<< fs) `shouldReturn` refVal2

doesFileExistSpec :: Spec
doesFileExistSpec =
  describe "doesFileExist" $ do
    it "should return true when file exists" $
      (doesFileExist "archive/index.txt" =<< sampleFileTree) `shouldReturn` True

    it "should return false when file does not exists" $ do
      (doesFileExist "archive/this-does-not-exist.txt" =<< sampleFileTree) `shouldReturn` False

doesDirExistSpec :: Spec
doesDirExistSpec =
  describe "doesDirExist" $ do
    it "should return true when dir exists" $
      (doesDirExist "archive/" =<< sampleFileTree) `shouldReturn` True

    it "should return false when dir does not exists" $ do
      (doesDirExist "archive/this-does-not-exist/" =<< sampleFileTree) `shouldReturn` False
      (doesDirExist "archive/this-does-not-exist.txt" =<< sampleFileTree) `shouldReturn` False

lookupDirSpec :: Spec
lookupDirSpec =
  describe "lookupDir" $ do
    it "should return files and dirs of root" $ do
      (lookupDir "vfs-root" =<< sampleFileTree) `shouldReturn` Just (Set.fromList ["archive/", "root.txt"])

    it "should return files and dirs" $ do
      (lookupDir "archive/" =<< sampleFileTree) `shouldReturn` Just (Set.fromList ["archive/feb/", "archive/index.txt", "archive/jan/"])
      (lookupDir "archive/feb/" =<< sampleFileTree) `shouldReturn` Just (Set.fromList ["archive/feb/1.txt", "archive/feb/extra/"])
      (lookupDir "archive/feb/extra/" =<< sampleFileTree) `shouldReturn` Just (Set.fromList ["archive/feb/extra/info.txt"])

lookupFileRefSpec :: Spec
lookupFileRefSpec =
  describe "lookupFileRef" $ do
    it "should return Nothing when querying dir" $ do
      (lookupFileRef "archive/" =<< sampleFileTree) `shouldReturn` Nothing

    it "should return Nothing when querying non existent entry" $ do
      (lookupFileRef "this-does-not-exit/" =<< sampleFileTree) `shouldReturn` Nothing
      (lookupFileRef "this-does-not-exit" =<< sampleFileTree) `shouldReturn` Nothing

    it "should return reference of file" $ do
      (lookupFileRef "root.txt" =<< sampleFileTree) `shouldReturn` refVal1
      (lookupFileRef "archive/index.txt" =<< sampleFileTree) `shouldReturn` refVal2

removeSpec :: Spec
removeSpec =
  describe "remove" $ do
    let removePath = remove . toSomePath

    it "should remove root level filepath node" $ do
      let fs = mkTree [("root.txt", refVal1)]
      (doesFileExist "root.txt" =<< removePath "root.txt" =<< fs) `shouldReturn` False

      let fs2 = mkTree [("root.txt", refVal1), ("a.txt", refVal2)]
      let removedRoot2 = removePath "root.txt" =<< fs2
      (doesFileExist "a.txt" =<< removedRoot2) `shouldReturn` True

    it "should remove nested filepath node" $ do
      let removedFs = removePath "archive/index.txt" =<< sampleFileTree
      (doesFileExist "archive/index.txt" =<< removedFs) `shouldReturn` False
      (doesFileExist "root.txt" =<< removedFs) `shouldReturn` True
      (doesFileExist "archive/jan/1.txt" =<< removedFs) `shouldReturn` True
      (doesFileExist "archive/feb/1.txt" =<< removedFs) `shouldReturn` True
      (doesFileExist "archive/feb/extra/info.txt" =<< removedFs) `shouldReturn` True

      let removedFs2 = removePath "archive/jan/1.txt" =<< sampleFileTree
      (doesFileExist "archive/jan/1.txt" =<< removedFs2) `shouldReturn` False
      (doesFileExist "root.txt" =<< removedFs2) `shouldReturn` True
      (doesFileExist "archive/index.txt" =<< removedFs2) `shouldReturn` True
      (doesFileExist "archive/feb/1.txt" =<< removedFs2) `shouldReturn` True
      (doesFileExist "archive/feb/extra/info.txt" =<< removedFs2) `shouldReturn` True

      let removedFs3 = removePath "archive/feb/extra/info.txt" =<< sampleFileTree
      (doesFileExist "archive/feb/extra/info.txt" =<< removedFs3) `shouldReturn` False
      (doesFileExist "root.txt" =<< removedFs3) `shouldReturn` True
      (doesFileExist "archive/index.txt" =<< removedFs3) `shouldReturn` True
      (doesFileExist "archive/jan/1.txt" =<< removedFs3) `shouldReturn` True
      (doesFileExist "archive/feb/1.txt" =<< removedFs3) `shouldReturn` True

      let removedFs4 = removePath "archive/feb/" =<< sampleFileTree
      (doesFileExist "archive/feb/extra/info.txt" =<< removedFs4) `shouldReturn` False
      (doesFileExist "archive/feb/1.txt" =<< removedFs4) `shouldReturn` False
      (doesFileExist "root.txt" =<< removedFs4) `shouldReturn` True
      (doesFileExist "archive/index.txt" =<< removedFs4) `shouldReturn` True
      (doesFileExist "archive/jan/1.txt" =<< removedFs4) `shouldReturn` True

      let removedFs5 = removePath "archive/" =<< sampleFileTree
      (doesFileExist "archive/index.txt" =<< removedFs5) `shouldReturn` False
      (doesFileExist "archive/jan/1.txt" =<< removedFs5) `shouldReturn` False
      (doesFileExist "archive/feb/1.txt" =<< removedFs5) `shouldReturn` False
      (doesFileExist "archive/feb/extra/info.txt" =<< removedFs5) `shouldReturn` False
      (doesFileExist "root.txt" =<< removedFs5) `shouldReturn` True

spec :: Spec
spec = do
  insertSpec
  removeSpec

  lookupDirSpec
  doesDirExistSpec

  lookupFileRefSpec
  doesFileExistSpec
