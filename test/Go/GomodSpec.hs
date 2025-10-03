{-# LANGUAGE QuasiQuotes #-}

module Go.GomodSpec (spec) where

import Control.Algebra (run)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.SemVer (version)
import Data.SemVer.Internal (Identifier (..))
import Data.Text (Text, empty)
import Data.Text.IO qualified as TIO
import DepTypes (DepType (GoType), Dependency (..), VerConstraint (CEq))
import Effect.Grapher (direct, evalGrapher)
import Graphing (Graphing (..))
import Strategy.Go.Gomod (Gomod (..), PackageVersion (..), Require (..), buildGraph, gomodParser)
import Strategy.Go.Types (graphingGolang)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (runParser)
import Text.RawString.QQ (r)

-- Helpers for building fixtures.

semver :: Int -> Int -> Int -> PackageVersion
semver x y z = Semantic $ version x y z [] []

dep :: Text -> Text -> Dependency
dep name revision =
  Dependency
    { dependencyType = GoType
    , dependencyName = name
    , dependencyVersion = Just (CEq revision)
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

-- Fixtures for testing.

trivialGomod :: Gomod
trivialGomod =
  Gomod
    { modName = "github.com/my/package"
    , modRequires =
        [ Require "github.com/pkg/one" (semver 1 0 0)
        , Require "github.com/pkg/two/v2" (semver 2 0 0)
        , Require "github.com/pkg/three/v3" (semver 3 0 0)
        ]
    , modReplaces = Map.fromList [("github.com/pkg/two/v2", Require "github.com/pkg/overridden" (NonCanonical "overridden"))]
    , modLocalReplaces = Map.empty
    , modExcludes = []
    }

trivialGraph :: Graphing Dependency
trivialGraph = run . evalGrapher $ do
  direct $ dep "github.com/pkg/one" "v1.0.0"
  direct $ dep "github.com/pkg/overridden" "overridden"
  direct $ dep "github.com/pkg/three/v3" "v3.0.0"

emptyGomod :: Gomod
emptyGomod =
  Gomod
    { modName = ""
    , modRequires = []
    , modReplaces = Map.empty
    , modLocalReplaces = Map.empty
    , modExcludes = []
    }

localReplaceGomod :: Gomod
localReplaceGomod =
  Gomod
    { modName = "github.com/my/package"
    , modRequires =
        [ Require "github.com/pkg/one" (semver 1 0 0)
        , Require "github.com/pkg/two/v2" (semver 2 0 0)
        , Require "github.com/pkg/three/v3" (semver 3 0 0)
        ]
    , modReplaces = Map.empty
    , modLocalReplaces = Map.fromList [("github.com/pkg/two/v2", "./foo")]
    , modExcludes = []
    }

localReplaceGraph :: Graphing Dependency
localReplaceGraph = run . evalGrapher $ do
  direct $ dep "github.com/pkg/one" "v1.0.0"
  direct $ dep "github.com/pkg/three/v3" "v3.0.0"

edgeCaseGomod :: Gomod
edgeCaseGomod =
  Gomod
    { modName = "test/package"
    , modRequires =
        [ Require "repo/name/A" (semver 1 0 0)
        , Require "repo/B" (Pseudo "000000000002")
        , Require "repo/C" (semver 1 1 0)
        , Require "repo/name/D" (semver 4 0 0)
        , Require "repo/E" (Semantic $ version 8 0 0 [] [IText "incompatible"])
        , Require "repo/F_underscore" (semver 1 0 0)
        ]
    , modReplaces =
        Map.fromList
          [ ("repo/B", Require "alias/repo/B" $ semver 0 1 0)
          , ("repo/C", Require "alias/repo/C" $ Pseudo "000000000003")
          , ("repo/E", Require "alias/repo/E" $ Pseudo "000000000005")
          , ("repo/F_underscore", Require "repo/F_underscore" $ semver 2 0 0)
          ]
    , modLocalReplaces =
        Map.fromList
          [ ("foo", "../foo")
          , ("bar", "/foo/bar/baz")
          ]
    , modExcludes =
        [ Require "repo/B" (semver 0 9 0)
        , Require "repo/C" (semver 1 0 0)
        , Require "repo/name/D" (semver 3 0 0)
        ]
    }

versionsGomod :: Gomod
versionsGomod =
  Gomod
    { modName = "github.com/hashicorp/nomad"
    , modRequires =
        [ Require
            { reqPackage = "github.com/containerd/go-cni"
            , reqVersion = Pseudo "d20b7eebc7ee"
            }
        , Require
            { reqPackage = "gopkg.in/tomb.v2"
            , reqVersion = Pseudo "14b3d72120e8"
            }
        , Require
            { reqPackage = "github.com/docker/docker"
            , reqVersion = Pseudo "7f8b4b621b5d"
            }
        , Require
            { reqPackage = "github.com/containernetworking/plugins"
            , reqVersion = Pseudo "2d6d46d308b2"
            }
        , Require
            { reqPackage = "github.com/ryanuber/columnize"
            , reqVersion = Pseudo "abc90934186a"
            }
        , Require
            { reqPackage = "github.com/opencontainers/runc"
            , reqVersion = Semantic (version 1 0 0 [IText "rc92"] [])
            }
        , Require
            { reqPackage = "honnef.co/go/tools"
            , reqVersion = Semantic (version 0 0 1 [INum 2020, INum 1, INum 4] [])
            }
        , Require
            { reqPackage = "hub.com/docker/distribution"
            , reqVersion = Semantic (version 2 7 1 [] [IText "incompatible"])
            }
        ]
    , modReplaces = mempty
    , modLocalReplaces = mempty
    , modExcludes = []
    }

versionsGraph :: Graphing Dependency
versionsGraph = run . evalGrapher $ do
  direct $ dep "github.com/containerd/go-cni" "d20b7eebc7ee"
  direct $ dep "gopkg.in/tomb.v2" "14b3d72120e8"
  direct $ dep "github.com/docker/docker" "7f8b4b621b5d"
  direct $ dep "github.com/containernetworking/plugins" "2d6d46d308b2"
  direct $ dep "github.com/ryanuber/columnize" "abc90934186a"
  direct $ dep "github.com/opencontainers/runc" "v1.0.0-rc92"
  direct $ dep "honnef.co/go/tools" "v0.0.1-2020.1.4"
  direct $ dep "hub.com/docker/distribution" "v2.7.1"

-- Actual test logic.

spec :: Spec
spec = do
  spec_buildGraph
  spec_parse

spec_buildGraph :: Spec
spec_buildGraph =
  describe "buildGraph" $ do
    it "constructs a trivial graph and performs overrides" $ do
      let result = buildGraph trivialGomod & graphingGolang & run
      result `shouldBe` trivialGraph

    it "renders different kinds of versions correctly" $ do
      let result = buildGraph versionsGomod & graphingGolang & run
      result `shouldBe` versionsGraph

    it "does not include locally replaced modules" $ do
      let result = buildGraph localReplaceGomod & graphingGolang & run
      result `shouldBe` localReplaceGraph

spec_parse :: Spec
spec_parse = do
  trivialInput <- runIO (TIO.readFile "test/Go/testdata/go.mod.trivial")
  edgecaseInput <- runIO (TIO.readFile "test/Go/testdata/go.mod.edgecases")
  versionsInput <- runIO (TIO.readFile "test/Go/testdata/go.mod.versions")

  describe "gomod parser" $ do
    it "parses a trivial example" $ do
      runParser gomodParser "" trivialInput `shouldParse` trivialGomod

    it "parses empty go.mod" $ do
      runParser gomodParser "" Data.Text.empty `shouldParse` emptyGomod

    it "parses each edge case" $ do
      runParser gomodParser "" edgecaseInput `shouldParse` edgeCaseGomod

    it "parses different kinds of module versions" $ do
      runParser gomodParser "" versionsInput `shouldParse` versionsGomod

    it "parses without failure when retract statements are included" $ do
      runParser gomodParser "" goModWithSingleRetract `shouldParse` gomodWithRetract
      runParser gomodParser "" goModWithMultipleRetract `shouldParse` gomodWithRetract
      runParser gomodParser "" goModWithMultipleRetractListLike `shouldParse` gomodWithRetract
      runParser gomodParser "" goModWithMultipleRetractComb `shouldParse` gomodWithRetract

    it "parses without failure when retract statements are included with comments" $ do
      runParser gomodParser "" goModWithRetractComment `shouldParse` gomodWithRetract
      runParser gomodParser "" goModWithRetractComment2 `shouldParse` gomodWithRetract
      runParser gomodParser "" goModWithRetractComment3 `shouldParse` gomodWithRetract
      runParser gomodParser "" goModWithRetractComment4 `shouldParse` gomodWithRetract

    it "fails to parse invalid go.mod" $ do
      runParser gomodParser "" `shouldFailOn` "invalid input"

gomodWithRetract :: Gomod
gomodWithRetract =
  Gomod
    { modName = "github.com/some/some"
    , modRequires =
        [ Require
            { reqPackage = "github.com/some/some"
            , reqVersion = Semantic (version 1 44 263 [] [])
            }
        ]
    , modReplaces = mempty
    , modLocalReplaces = mempty
    , modExcludes = []
    }

goModWithSingleRetract :: Text
goModWithSingleRetract =
  [r|module github.com/some/some

go 1.19

require (
	github.com/some/some v1.44.263
)

retract v0.76.2
|]

goModWithMultipleRetract :: Text
goModWithMultipleRetract =
  [r|module github.com/some/some

go 1.19

require (
	github.com/some/some v1.44.263
)

retract (
	v0.76.2
	v0.76.1
	v0.65.0
)
|]

goModWithMultipleRetractListLike :: Text
goModWithMultipleRetractListLike =
  [r|module github.com/some/some

go 1.19

require (
	github.com/some/some v1.44.263
)

retract [v0.76.2, v0.76.3] 
|]

goModWithMultipleRetractComb :: Text
goModWithMultipleRetractComb =
  [r|module github.com/some/some

go 1.19

require (
	github.com/some/some v1.44.263
)

retract (
	v0.76.2
	v0.76.1
	v0.65.0
  [v0.10.1, v0.10.2]
)|]

goModWithRetractComment :: Text
goModWithRetractComment =
  [r|module github.com/some/some

require (
	github.com/some/some v1.44.263
)

retract v0.76.2 // comment.
|]

goModWithRetractComment2 :: Text
goModWithRetractComment2 =
  [r|module github.com/some/some

require (
	github.com/some/some v1.44.263
)

retract  [v0.10.1, v0.10.2] // some comment
|]

goModWithRetractComment3 :: Text
goModWithRetractComment3 =
  [r|module github.com/some/some

require (
	github.com/some/some v1.44.263
)

retract (
	v0.76.2 // some comment.
	v0.76.1
	v0.65.0
)|]

goModWithRetractComment4 :: Text
goModWithRetractComment4 =
  [r|module github.com/some/some

require (
	github.com/some/some v1.44.263
)

retract (
	v0.76.2 // some comment
	v0.76.1
	v0.65.0
  [v0.10.1, v0.10.2] // some comment
)|]
