module Conan.ConanGraphSpec (spec) where

import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Extra (TextLike (TextLike))
import Data.Map (empty, fromList)
import Data.Set qualified as Set
import DepTypes (DepEnvironment (..), DepType (..), Dependency (..), VerConstraint (..))
import Strategy.Conan.ConanGraph (
  ConanGraph (..),
  ConanGraphNode (..),
  ConanGraphNodeContext (..),
  ConanPackageType (..),
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
    { nodes = [consumerNode, zlibNode, opensslNode, cmakeNode]
    , root = fromList [("0", "")]
    }

consumerNode :: ConanGraphNode
consumerNode =
  ConanGraphNode
    { ref = TextLike ""
    , nodeid = TextLike "0"
    , packageId = "e982649002579f1faa063c390c3788c91140d20d"
    , name = "conanfile.py"
    , version = ""
    , context = HostContext
    , settings =
        fromList
          [ ("os", "Macos")
          , ("arch", "armv8")
          , ("compiler", "clang")
          , ("compiler.cppstd", "gnu14")
          , ("compiler.libcxx", "libc++")
          , ("compiler.version", "13")
          , ("build_type", "Release")
          ]
    , options = Data.Map.empty
    , packgeType = Unknown "unknown"
    , sourceFolder = Just "/Users/dev/code/conanexample/examples2/tutorial/consuming_packages/conanfile_py"
    , buildFolder = Just "/Users/dev/code/conanexample/examples2/tutorial/consuming_packages/conanfile_py"
    , test = False
    , requires =
        fromList
          [ ("1", "zlib/1.2.13#e377bee636333ae348d51ca90874e353")
          , ("2", "openssl/3.1.0#8eadf484563de6cdd24daafb9c3813db")
          , ("3", "cmake/3.22.6#32cced101c6df0fab43e8d00bd2483eb")
          ]
    }

zlibNode :: ConanGraphNode
zlibNode =
  ConanGraphNode
    { ref = TextLike "zlib/1.2.13#e377bee636333ae348d51ca90874e353"
    , nodeid = TextLike "1"
    , name = "zlib"
    , packageId = "6ee94108e5a809f66e5396a0549a9ff4ed7621e8"
    , version = "1.2.13"
    , context = HostContext
    , settings =
        fromList
          [ ("os", "Macos")
          , ("arch", "armv8")
          , ("compiler", "clang")
          , ("compiler.version", "13")
          , ("build_type", "Release")
          ]
    , options =
        fromList
          [ ("fPIC", Just "True")
          , ("shared", Just "False")
          ]
    , packgeType = StaticLibrary
    , sourceFolder = Nothing
    , buildFolder = Nothing
    , test = False
    , requires = Data.Map.empty
    }

opensslNode :: ConanGraphNode
opensslNode =
  ConanGraphNode
    { ref = TextLike "openssl/3.1.0#8eadf484563de6cdd24daafb9c3813db"
    , nodeid = TextLike "2"
    , name = "openssl"
    , packageId = "c6f0d27a6f6fc6a1f1861a5b4bf6b5001d4a6565"
    , version = "3.1.0"
    , context = HostContext
    , settings =
        fromList
          [ ("os", "Macos")
          , ("arch", "armv8")
          , ("compiler", "clang")
          , ("compiler.version", "13")
          , ("build_type", "Release")
          ]
    , options =
        fromList
          [ ("386", Just "False")
          , ("enable_weak_ssl_ciphers", Just "False")
          , ("openssldir", Nothing)
          , ("shared", Just "False")
          ]
    , packgeType = StaticLibrary
    , sourceFolder = Nothing
    , buildFolder = Nothing
    , test = False
    , requires =
        fromList
          [ ("1", "zlib/1.2.13#e377bee636333ae348d51ca90874e353")
          ]
    }

cmakeNode :: ConanGraphNode
cmakeNode =
  ConanGraphNode
    { ref = TextLike "cmake/3.22.6#32cced101c6df0fab43e8d00bd2483eb"
    , nodeid = TextLike "3"
    , name = "cmake"
    , packageId = "9e5323c65b94ae38c3c733fe12637776db0119a5"
    , version = "3.22.6"
    , context = BuildContext
    , settings =
        fromList
          [ ("os", "Macos")
          , ("arch", "armv8")
          ]
    , options = Data.Map.empty
    , packgeType = Application
    , sourceFolder = Nothing
    , buildFolder = Nothing
    , test = False
    , requires = Data.Map.empty
    }

nodeWithSrc :: ConanGraphNode
nodeWithSrc =
  ConanGraphNode
    { ref = TextLike "nodeWithSrc/1.0.0#pkgId"
    , nodeid = TextLike "3"
    , name = "nodeWithSrc"
    , packageId = "pkgId"
    , version = "1.0.0"
    , context = HostContext
    , settings = Data.Map.empty
    , options = Data.Map.empty
    , packgeType = Library
    , sourceFolder = Just "src_dir"
    , buildFolder = Just "build_dir"
    , test = False
    , requires = Data.Map.empty
    }

sharedLib :: ConanGraphNode
sharedLib =
  ConanGraphNode
    { ref = TextLike "nodeWithSharedLib/1.0.0#pkgId"
    , nodeid = TextLike "3"
    , name = "nodeWithSharedLib"
    , packageId = "pkgId"
    , version = "1.0.0"
    , context = HostContext
    , settings = Data.Map.empty
    , options = Data.Map.empty
    , packgeType = SharedLibrary
    , sourceFolder = Just "src_dir"
    , buildFolder = Just "build_dir"
    , test = False
    , requires = Data.Map.empty
    }
