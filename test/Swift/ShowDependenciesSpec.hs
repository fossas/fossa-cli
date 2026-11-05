module Swift.ShowDependenciesSpec (
  spec,
) where

import Data.Aeson (eitherDecodeFileStrict')
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DepTypes (DepType (GitType, SwiftType, UnresolvedPathType), Dependency (..), VerConstraint (CEq))
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Swift.PackageResolved (SwiftResolvedPackage (..))
import Strategy.Swift.ShowDependencies (ShowDependenciesPackage (..), buildGraph)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

dep :: DepType -> Text -> Maybe Text -> Dependency
dep depType name version =
  Dependency
    { dependencyType = depType
    , dependencyName = name
    , dependencyVersion = CEq <$> version
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = mempty
    }

swiftLog, swiftNio, swiftAtomics, swiftCollections, swiftNumerics, localPkg :: Dependency
swiftLog = dep SwiftType "https://github.com/apple/swift-log.git" (Just "1.17.0")
swiftNio = dep SwiftType "https://github.com/apple/swift-nio.git" (Just "2.105.0")
swiftAtomics = dep SwiftType "https://github.com/apple/swift-atomics.git" (Just "1.3.1")
swiftCollections = dep SwiftType "https://github.com/apple/swift-collections.git" (Just "1.7.5")
swiftNumerics = dep SwiftType "https://github.com/apple/swift-numerics.git" (Just "1.0.2")
localPkg = dep UnresolvedPathType "/home/user/local-pkg" Nothing

-- Branch and revision dependencies have no version in the tree.
swiftAlgorithms, swiftArgumentParser :: Maybe Text -> Dependency
swiftAlgorithms = dep GitType "https://github.com/apple/swift-algorithms.git"
swiftArgumentParser = dep GitType "https://github.com/apple/swift-argument-parser.git"

pins :: Map.Map Text SwiftResolvedPackage
pins =
  Map.fromList
    [ pin "https://github.com/apple/swift-algorithms.git" (Just "main") (Just "5b7143f8e291dee0e14c118fd0212487f0b37af5") Nothing
    , pin "https://github.com/apple/swift-argument-parser.git" (Just "1.3.0") (Just "c8ed701b513cf5177118a175d85fbbbcd707ab41") Nothing
    , pin "https://github.com/apple/swift-log.git" Nothing (Just "e3da04078777e6a1b72ce19981fbd6539b0847d8") (Just "1.17.0")
    ]
  where
    pin url branch revision version = (url, SwiftResolvedPackage "unused" url branch revision version)

spec :: Spec
spec = do
  tree <- runIO $ eitherDecodeFileStrict' "test/Swift/testdata/show-dependencies.json"

  describe "show-dependencies json" $ do
    it "parses the tree" $ do
      let names = map packageIdentity . packageDependencies <$> tree
      names `shouldBe` Right ["swift-log", "swift-nio", "swift-algorithms", "swift-argument-parser", "swift-numerics", "local-pkg"]

  describe "buildGraph" $ do
    let graph = either (const mempty) (buildGraph pins) tree
    let algorithms = swiftAlgorithms (Just "5b7143f8e291dee0e14c118fd0212487f0b37af5")
    let argumentParser = swiftArgumentParser (Just "c8ed701b513cf5177118a175d85fbbbcd707ab41")

    it "reports the root's dependencies as direct, including local packages" $
      expectDirect [swiftLog, swiftNio, algorithms, argumentParser, swiftNumerics, localPkg] graph

    it "reports every package once, however many packages depend on it" $
      expectDeps [swiftLog, swiftNio, swiftAtomics, swiftCollections, algorithms, swiftNumerics, argumentParser, localPkg] graph

    it "reports the edges of the tree" $
      expectEdges
        [ (swiftNio, swiftAtomics)
        , (swiftNio, swiftCollections)
        , (algorithms, swiftNumerics)
        , (localPkg, swiftCollections)
        ]
        graph

    it "leaves branch and revision dependencies unversioned without Package.resolved" $ do
      let graphWithoutPins = either (const mempty) (buildGraph Map.empty) tree
      expectDirect [swiftLog, swiftNio, swiftAlgorithms Nothing, swiftArgumentParser Nothing, swiftNumerics, localPkg] graphWithoutPins
