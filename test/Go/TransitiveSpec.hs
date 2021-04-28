module Go.TransitiveSpec (spec) where

import Control.Algebra (run)
import DepTypes
import GraphUtil
import Strategy.Go.Transitive
import Strategy.Go.Types (graphingGolang)
import Test.Hspec

spec :: Spec
spec = spec_packageToModule

-- HACK(fossas/team-analysis#514) see Strategy.Go.Transitive for more details
--
-- Ensure that packages are correctly mapped to their modules, and that packages
-- aren't dropped from the dependency graph
spec_packageToModule :: Spec
spec_packageToModule =
  describe "normalizeImportsToModules" $ do
    it "should map package imports to their modules" $ do
      let result = normalizeImportsToModules testPackages
      result `shouldBe` normalizedPackages

    it "should prevent packages from appearing in the final graph" $ do
      let graph = run $ graphingGolang (graphTransitive (normalizeImportsToModules testPackages))
      expectDeps [fooDep, barDep, bazDep] graph
      expectEdges [(fooDep, barDep), (barDep, bazDep)] graph

    it "should be a no-op for non-module packages" $ do
      normalizeImportsToModules nonModulePackages `shouldBe` nonModulePackages

nonModulePackages :: [Package]
nonModulePackages =
  [ Package
      { packageImportPath = "github.com/example/foo"
      , packageModule = Nothing
      , packageImports =
          Just
            [ "github.com/example/nonmodule/foo"
            ]
      , packageSystem = Nothing
      }
  , Package
      { packageImportPath = "github.com/example/nonmodule/foo"
      , packageModule = Nothing
      , packageImports = Nothing
      , packageSystem = Nothing
      }
  ]

testPackages :: [Package]
testPackages =
  [ Package
      { packageImportPath = "github.com/example/foo"
      , packageModule = Nothing
      , packageImports =
          Just
            [ "github.com/example/bar/some/package"
            ]
      , packageSystem = Nothing
      }
  , Package
      { packageImportPath = "github.com/example/bar/some/package"
      , packageModule =
          Just
            Module
              { modPath = "github.com/example/bar"
              , modVersion = Nothing
              }
      , packageImports =
          Just
            [ "github.com/example/baz"
            , "github.com/example/baz/other"
            ]
      , packageSystem = Nothing
      }
  , Package
      { packageImportPath = "github.com/example/baz/other"
      , packageModule =
          Just
            Module
              { modPath = "github.com/example/baz"
              , modVersion = Nothing
              }
      , packageImports = Nothing
      , packageSystem = Nothing
      }
  , Package
      { packageImportPath = "github.com/example/baz"
      , packageModule =
          Just
            Module
              { modPath = "github.com/example/baz"
              , modVersion = Nothing
              }
      , packageImports = Nothing
      , packageSystem = Nothing
      }
  ]

normalizedPackages :: [Package]
normalizedPackages =
  [ Package
      { packageImportPath = "github.com/example/foo"
      , packageModule = Nothing
      , packageImports =
          Just
            [ "github.com/example/bar"
            ]
      , packageSystem = Nothing
      }
  , Package
      { packageImportPath = "github.com/example/bar/some/package"
      , packageModule =
          Just
            Module
              { modPath = "github.com/example/bar"
              , modVersion = Nothing
              }
      , packageImports =
          Just
            [ "github.com/example/baz"
            , "github.com/example/baz"
            ]
      , packageSystem = Nothing
      }
  , Package
      { packageImportPath = "github.com/example/baz/other"
      , packageModule =
          Just
            Module
              { modPath = "github.com/example/baz"
              , modVersion = Nothing
              }
      , packageImports = Nothing
      , packageSystem = Nothing
      }
  , Package
      { packageImportPath = "github.com/example/baz"
      , packageModule =
          Just
            Module
              { modPath = "github.com/example/baz"
              , modVersion = Nothing
              }
      , packageImports = Nothing
      , packageSystem = Nothing
      }
  ]

fooDep :: Dependency
fooDep =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/example/foo"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = mempty
    }

barDep :: Dependency
barDep =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/example/bar"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = mempty
    }

bazDep :: Dependency
bazDep =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/example/baz"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = mempty
    }
