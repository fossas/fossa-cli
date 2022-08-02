import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.FileTree.IndexFileTree (
  SomeFileTree (..),
  empty,
  insert,
  toSomePath,
 )
import Data.Foldable (Foldable (foldr'))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Test.Tasty.Bench (bench, defaultMain, env, nf)

hundredKFilePaths :: FilePath
hundredKFilePaths = "bench/benchdata/filepaths.txt"

main :: IO ()
main = do
  -- Tar entries do not have `/` prefix in their absolute paths, but bench data does.
  -- Remove them to mimic TarEntries fmt.
  txt <- map rmLeadingSlash <$> fmap Text.lines (TextIO.readFile hundredKFilePaths)
  defaultMain
    [ env
        ( evaluate
            (force txt)
        )
        $ \largeSetsOfFiles ->
          bench "100k path insertions" $ nf mkTree largeSetsOfFiles
    ]

-- * Helpers

rmLeadingSlash :: Text -> Text
rmLeadingSlash t = fromMaybe t $ Text.stripPrefix "/" t

mkTree :: [Text] -> (SomeFileTree Int)
mkTree = foldr' (\p tree -> insert (toSomePath p) Nothing tree) empty
