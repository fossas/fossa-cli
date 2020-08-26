module NuGet.PackagesConfigSpec
  ( spec
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import DepTypes
import GraphUtil
import Strategy.NuGet.PackagesConfig
import Parse.XML
import Test.Hspec

dependencyOne :: Dependency
dependencyOne = Dependency { dependencyType = NuGetType
                        , dependencyName = "one"
                        , dependencyVersion = Just (CEq "1.0.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = []
                        , dependencyTags = M.empty
                        }

dependencyTwo :: Dependency
dependencyTwo = Dependency { dependencyType = NuGetType
                        , dependencyName = "two"
                        , dependencyVersion = Just (CEq "2.0.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = []
                        , dependencyTags = M.empty
                        }

packagesConfig :: PackagesConfig
packagesConfig = PackagesConfig depList

depList :: [NuGetDependency]
depList = [NuGetDependency "one" "1.0.0", NuGetDependency "two" "2.0.0"]

spec :: Spec
spec = do
  nuspecFile <- runIO (TIO.readFile "test/NuGet/testdata/packages.config")

  describe "packages.config analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case parseXML nuspecFile of
        Right project -> (deps project) `shouldContain` depList
        Left err -> expectationFailure (T.unpack ("could not parse packages.config file" <> xmlErrorPretty err))

    it "constructs an accurate graph" $ do
          let graph = buildGraph packagesConfig
          expectDeps [dependencyOne, dependencyTwo] graph
          expectDirect [dependencyOne, dependencyTwo] graph
          expectEdges [] graph
