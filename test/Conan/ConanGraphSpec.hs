module Conan.ConanGraphSpec (spec) where

import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Extra (TextLike (TextLike))
import Data.Map (empty, fromList)
import Data.Set qualified as Set
import DepTypes (DepEnvironment (..), DepType (..), Dependency (..), VerConstraint (..))
import Strategy.Conan.ConanGraph
  ( ConanGraph (..),
    ConanGraphNode (..),
    ConanGraphNodeContext (..),
    toDependency,
  )
import Test.Hspec qualified as T

spec :: T.Spec
spec = do
  simpleGraph <- T.runIO $ eitherDecodeFileStrict' "test/Conan/testdata/simple.json"

  T.describe "conan output" $
    T.it "should parse graph" $ do
      case simpleGraph of
        Left err -> fail err
        Right g -> g `T.shouldBe` expectedSimpleGraph

  T.describe "dependency conversion" $ do
    T.it "should use context and test attribute to determine environemnt" $ do
      toDependency cmakeNode
        `T.shouldBe` ( Dependency
                         ConanType
                         "cmake"
                         (Just $ CEq "3.22.6?arch=armv8&os=Macos&package_id=9e5323c65b94ae38c3c733fe12637776db0119a5")
                         mempty
                         (Set.singleton EnvDevelopment)
                         mempty
                     )

      toDependency zlibNode
        `T.shouldBe` ( Dependency
                         ConanType
                         "zlib"
                         (Just $ CEq "1.2.13?arch=armv8&build_type=Release&compiler=clang&compiler.version=13&os=Macos&package_id=6ee94108e5a809f66e5396a0549a9ff4ed7621e8")
                         mempty
                         (Set.singleton EnvProduction)
                         mempty
                     )

    T.it "should include location, if source directory is provided, for non-shared lib package type" $ do
      toDependency nodeWithSrc
        `T.shouldBe` ( Dependency
                         ConanType
                         "nodeWithSrc"
                         (Just $ CEq "1.0.0?package_id=pkgId")
                         ["src_dir"]
                         (Set.singleton EnvProduction)
                         mempty
                     )

    T.it "should include location, if build directory is provided for shared lib" $ do
      toDependency sharedLib
        `T.shouldBe` ( Dependency
                         ConanType
                         "nodeWithSharedLib"
                         (Just $ CEq "1.0.0?package_id=pkgId")
                         ["build_dir"]
                         (Set.singleton EnvProduction)
                         mempty
                     )

expectedSimpleGraph :: ConanGraph
expectedSimpleGraph =
  ConanGraph
    { nodes =
        fromList
          [ ("0", consumerNode),
            ("1", zlibNode),
            ("2", opensslNode),
            ("3", cmakeNode)
          ],
      root = fromList [("0", "")]
    }

consumerNode :: ConanGraphNode
consumerNode =
  ConanGraphNode
    { ref = TextLike "",
      nodeid = TextLike "0",
      packageId = Just "e982649002579f1faa063c390c3788c91140d20d",
      name = Just "conanfile.py",
      version = Just "",
      context = HostContext,
      test = False,
      dependencies = Data.Map.empty
    }

zlibNode :: ConanGraphNode
zlibNode =
  ConanGraphNode
    { ref = TextLike "zlib/1.2.13#e377bee636333ae348d51ca90874e353",
      nodeid = TextLike "1",
      name = Just "zlib",
      packageId = Just "6ee94108e5a809f66e5396a0549a9ff4ed7621e8",
      version = Just "1.2.13",
      context = HostContext,
      test = False,
      dependencies = Data.Map.empty
    }

opensslNode :: ConanGraphNode
opensslNode =
  ConanGraphNode
    { ref = TextLike "openssl/3.1.0#8eadf484563de6cdd24daafb9c3813db",
      nodeid = TextLike "2",
      name = Just "openssl",
      packageId = Just "c6f0d27a6f6fc6a1f1861a5b4bf6b5001d4a6565",
      version = Just "3.1.0",
      context = HostContext,
      test = False,
      dependencies = Data.Map.empty
    }

cmakeNode :: ConanGraphNode
cmakeNode =
  ConanGraphNode
    { ref = TextLike "cmake/3.22.6#32cced101c6df0fab43e8d00bd2483eb",
      nodeid = TextLike "3",
      name = Just "cmake",
      packageId = Just "9e5323c65b94ae38c3c733fe12637776db0119a5",
      version = Just "3.22.6",
      context = BuildContext,
      test = False,
      dependencies = Data.Map.empty
    }

nodeWithSrc :: ConanGraphNode
nodeWithSrc =
  ConanGraphNode
    { ref = TextLike "nodeWithSrc/1.0.0#pkgId",
      nodeid = TextLike "3",
      name = Just "nodeWithSrc",
      packageId = Just "pkgId",
      version = Just "1.0.0",
      context = HostContext,
      test = False,
      dependencies = Data.Map.empty
    }

sharedLib :: ConanGraphNode
sharedLib =
  ConanGraphNode
    { ref = TextLike "nodeWithSharedLib/1.0.0#pkgId",
      nodeid = TextLike "3",
      name = Just "nodeWithSharedLib",
      packageId = Just "pkgId",
      version = Just "1.0.0",
      context = HostContext,
      test = False,
      dependencies = Data.Map.empty
    }
