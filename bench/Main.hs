import Test.Tasty.Bench
import qualified Data.Text.IO as TextIO
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.FileTree.IndexFileTree (allLeadingPaths, zippedPath, make, SomeFileTree(..))
import Text.Pretty.Simple (pPrint)
import qualified Data.HashTable.IO as H

filepaths :: FilePath
filepaths = "bench/benchdata/filepaths_cents.txt"

filesWithoutLeadingSlash :: Text -> Text
filesWithoutLeadingSlash t = fromMaybe t $ Text.stripPrefix "/" t

mk :: [Text] -> IO SomeFileTree
mk = make

main :: IO ()
main = do
    -- Tar entries do not have `/` prefix in their absolute paths.
    -- Remove them to mimic TarEntries fmt. 
    benchPaths <- map filesWithoutLeadingSlash <$> fmap Text.lines (TextIO.readFile filepaths)

    let splitPaths = map (zippedPath . allLeadingPaths)

    defaultMain
        [ bgroup
            "indexed file tree"
            [   bench "splits by line-break" $ whnfAppIO mk benchPaths
            ]
        ]
