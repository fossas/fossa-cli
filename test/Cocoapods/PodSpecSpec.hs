module Cocoapods.PodSpecSpec (spec) where
import DepTypes
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import Test.Hspec qualified as T
import Strategy.Cocoapods.Podspecs (buildGraph, parsePodspecFile)
import GraphUtil
import qualified Data.Text
import Path (Path, Abs, File)

dependencyOne :: Dependency
dependencyOne =
  Dependency
    { dependencyType = GemType
    , dependencyName = "dep-one"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = ["temp@12345"]
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

spec :: T.Spec
spec = do
  specFile <- T.runIO (TIO.readFile "test/Cocoapods/testdata/dependencies.podspec")

  T.describe "podspec analyzer" $
    T.it "produces expected output" $ do
      let graph = buildGraph (parsePodspecFile specFile)

      expectDeps [dependencyOne] graph