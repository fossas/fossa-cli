module Go.TransitiveSpec (spec) where

import Control.Algebra (run)
import DepTypes (DepType (GoType), Dependency (..), VerConstraint (CEq))
import GraphUtil (expectDeps, expectEdges)
import Strategy.Go.Transitive as Transitive (
  GoListPkgImportPath (GoListPkgImportPath),
  GoPackageReplacement (..),
  Module (Module, modPath, modReplacement, modVersion),
  NormalizedImportPath (NormalizedImportPath),
  Package (..),
  graphTransitive,
  normalizeImportPaths,
 )

import Data.Coerce (coerce)
import Strategy.Go.Types (graphingGolang)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  spec_packageToModule
  performsPackageReplacementSpec

performsPackageReplacementSpec :: Spec
performsPackageReplacementSpec =
  describe "Package list module replacements" $ do
    it "replaces module name and version when the replacement isn't in an import list" $ do
      let normalized = normalizeImportPaths unimportedPackageReplacement
          graph = run $ graphingGolang (graphTransitive normalized)
      expectDeps [replacedModuleDep, nonReplacedPackageDep] graph
      expectEdges [(replacedModuleDep, nonReplacedPackageDep)] graph

    it "replaces module name and version when the replacement is in an import list" $ do
      let normalized = normalizeImportPaths importedPackageReplacement
          graph = run $ graphingGolang (graphTransitive normalized)
      expectDeps [replacedModuleDep, nonReplacedPackageDep] graph
      expectEdges [(nonReplacedPackageDep, replacedModuleDep)] graph

replacedModuleDep :: Dependency
replacedModuleDep =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/example/bar"
    , dependencyVersion = Just $ CEq "2.0.0"
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = mempty
    }

nonReplacedPackageDep :: Dependency
nonReplacedPackageDep =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/example/baz"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = mempty
    }

unimportedPackageReplacement :: [Package GoListPkgImportPath]
unimportedPackageReplacement =
  [ Package
      { packageImportPath = "github.com/example/foo/inner-package"
      , packageModule =
          Just
            Module
              { modPath = "github.com/example/foo"
              , modVersion = Just "1.0.0"
              , modReplacement =
                  Just
                    GoPackageReplacement
                      { Transitive.pathReplacement = "github.com/example/bar"
                      , Transitive.versionReplacement = "2.0.0"
                      }
              }
      , packageImports = Just ["github.com/example/baz"]
      , packageSystem = Nothing
      }
  , Package
      { packageImportPath = "github.com/example/baz"
      , packageModule = Nothing
      , packageImports = Nothing
      , packageSystem = Nothing
      }
  ]

importedPackageReplacement :: [Package GoListPkgImportPath]
importedPackageReplacement =
  [ Package
      { packageImportPath = "github.com/example/foo/inner-package"
      , packageModule =
          Just
            Module
              { modPath = "github.com/example/foo"
              , modVersion = Just "1.0.0"
              , modReplacement =
                  Just
                    GoPackageReplacement
                      { Transitive.pathReplacement = "github.com/example/bar"
                      , Transitive.versionReplacement = "2.0.0"
                      }
              }
      , packageImports = Nothing
      , packageSystem = Nothing
      }
  , Package
      { packageImportPath = "github.com/example/baz"
      , packageModule = Nothing
      , packageImports = Just ["github.com/example/foo/inner-package"]
      , packageSystem = Nothing
      }
  ]

-- HACK(fossas/team-analysis#514) see Strategy.Go.Transitive for more details
--
-- Ensure that packages are correctly mapped to their modules, and that packages
-- aren't dropped from the dependency graph
spec_packageToModule :: Spec
spec_packageToModule =
  describe "normalizeImportPaths" $ do
    it "should map package imports to their modules" $ do
      let result = normalizeImportPaths testPackages
      result `shouldBe` normalizedPackages

    it "should prevent packages from appearing in the final graph" $ do
      let graph = run $ graphingGolang (graphTransitive (normalizeImportPaths testPackages))
      expectDeps [fooDep, barDep, bazDep] graph
      expectEdges [(fooDep, barDep), (barDep, bazDep)] graph

    it "should be a no-op for non-module packages" $ do
      normalizeImportPaths nonModulePackages `shouldBe` coerce nonModulePackages

nonModulePackages :: [Package GoListPkgImportPath]
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

testPackages :: [Package GoListPkgImportPath]
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
              , modReplacement = Nothing
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
              , modReplacement = Nothing
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
              , modReplacement = Nothing
              }
      , packageImports = Nothing
      , packageSystem = Nothing
      }
  ]

normalizedPackages :: [Package NormalizedImportPath]
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
      { packageImportPath = "github.com/example/bar"
      , packageModule =
          Just
            Module
              { modPath = "github.com/example/bar"
              , modVersion = Nothing
              , modReplacement = Nothing
              }
      , packageImports =
          Just
            [ "github.com/example/baz"
            , "github.com/example/baz"
            ]
      , packageSystem = Nothing
      }
  , Package
      { packageImportPath = "github.com/example/baz"
      , packageModule =
          Just
            Module
              { modPath = "github.com/example/baz"
              , modVersion = Nothing
              , modReplacement = Nothing
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
              , modReplacement = Nothing
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
    , dependencyEnvironments = mempty
    , dependencyTags = mempty
    }

barDep :: Dependency
barDep =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/example/bar"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = mempty
    }

bazDep :: Dependency
bazDep =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/example/baz"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = mempty
    }
