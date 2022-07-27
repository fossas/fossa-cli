import Data.FileTree.IndexFileTree (
  SomeFileTree (..),
  allLeadingPaths,
  empty,
  insert,
  toSomePath,
  zippedPath,
 )
import Data.HashTable.IO qualified as H
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf, whnfAppIO)

hundredKFilePaths :: FilePath
hundredKFilePaths = "bench/benchdata/filepaths.txt"

main :: IO ()
main = do
  -- Tar entries do not have `/` prefix in their absolute paths, but bench data does.
  -- Remove them to mimic TarEntries fmt.
  benchPaths <- map rmLeadingSlash <$> fmap Text.lines (TextIO.readFile hundredKFilePaths)

  defaultMain
    [ bgroup
        "indexed file tree"
        [ bench "splitting 100k filepaths components" $ nf (map (zippedPath . allLeadingPaths)) benchPaths
        , bench "inserting 1k filepaths" $ whnfAppIO mkTree (take 1000 benchPaths)
        , bench "inserting 10k filepaths" $ whnfAppIO mkTree (take 10000 benchPaths)
        , bench "inserting 100k filepaths" $ whnfAppIO mkTree (take 100000 benchPaths)
        ]
    ]

-- * Helpers

rmLeadingSlash :: Text -> Text
rmLeadingSlash t = fromMaybe t $ Text.stripPrefix "/" t

mkTree :: [Text] -> IO (SomeFileTree Int)
mkTree = foldrM (\p tree -> insert (toSomePath p) Nothing tree) empty
  where
    -- monadic foldr
    foldrM :: Monad m => (a -> b -> m b) -> m b -> [a] -> m b
    foldrM f = foldr ((=<<) . f)