import Test.Tasty.Bench
import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text

filepaths :: FilePath
filepaths = "bench/benchdata/filepaths.txt"

main :: IO ()
main = do
    benchPaths <- TextIO.readFile filepaths
    defaultMain
        [ bgroup
            "indexed file tree"
            [   bench "splits by line-break" $ nf Text.lines benchPaths
            ]
        ]
