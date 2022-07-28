import Control.DeepSeq (force)
import Control.Exception (evaluate)
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
import System.IO.Unsafe
import Test.Tasty.Bench (bench, bgroup, defaultMain, env, nf, whnfAppIO)

hundredKFilePaths :: FilePath
hundredKFilePaths = "bench/benchdata/filepaths.txt"

main :: IO ()
main = do
  -- Tar entries do not have `/` prefix in their absolute paths, but bench data does.
  -- Remove them to mimic TarEntries fmt.

  -- paths <- (map rmLeadingSlash <$> fmap Text.lines (TextIO.readFile hundredKFilePaths))
  -- benchPaths <- map rmLeadingSlash <$> fmap Text.lines (TextIO.readFile hundredKFilePaths)

  defaultMain
    [ env
        ( evaluate
            ( force
                ( take 27000 $
                    unsafePerformIO $
                      map rmLeadingSlash <$> fmap Text.lines (TextIO.readFile hundredKFilePaths)
                )
            )
        )
        $ \centOsFileChangeSet ->
          bench "centOsFileChangeSet" $ whnfAppIO mkTree centOsFileChangeSet
    ]

-- defaultMain
--   [ bgroup
--       "indexed file tree" [
--       -- [ bench "splitting 100k filepaths components" $ nf (map (zippedPath . allLeadingPaths)) benchPaths
--       -- , bench "inserting 1k filepaths" $ whnfAppIO mkTree (take 1000 benchPaths)
--       -- , bench "inserting 10k filepaths" $ whnfAppIO mkTree (take 10000 benchPaths)
--         bench "inserting 100k filepaths" $ whnfAppIO $ do

--           pure (mkTree paths)
--       ]
--   ]

-- * Helpers

rmLeadingSlash :: Text -> Text
rmLeadingSlash t = fromMaybe t $ Text.stripPrefix "/" t

mkTreeIO :: IO [Text] -> IO (SomeFileTree Int)
mkTreeIO entries = mkTree =<< entries

mkTree :: [Text] -> IO (SomeFileTree Int)
mkTree = foldrM (\p tree -> insert (toSomePath p) Nothing tree) empty
  where
    -- monadic foldr
    foldrM :: Monad m => (a -> b -> m b) -> m b -> [a] -> m b
    foldrM f = foldr ((=<<) . f)