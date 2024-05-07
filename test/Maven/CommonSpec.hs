module Maven.CommonSpec (
  spec,
) where

import Data.Text (Text)
import DepTypes (DepType (MavenType), Dependency (..), VerConstraint (CEq))

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Discovery.Filters (MavenScopeFilterPredicate (..), MavenScopeFilters (MavenScopeExcludeFilters, MavenScopeOnlyFilters))
import GraphUtil
import Graphing qualified
import Strategy.Maven.Common (MavenDependency (..), filterMavenDependencyByScope, filterMavenSubmodules)
import Test.Hspec (Spec, describe, it, shouldBe)

createDepFromScopes :: [Text] -> MavenDependency
createDepFromScopes scopes = do
  let dep =
        Dependency
          { dependencyType = MavenType
          , dependencyName = "mygroup:packageOne"
          , dependencyVersion = Just (CEq "1.0.0")
          , dependencyLocations = []
          , dependencyEnvironments = Set.fromList []
          , dependencyTags = Map.fromList [("scopes", scopes)]
          }
  MavenDependency dep (Set.fromList scopes) mempty

createDepFromName :: Text -> [Text] -> MavenDependency
createDepFromName depName submodules = do
  let dep =
        Dependency
          { dependencyType = MavenType
          , dependencyName = depName
          , dependencyVersion = Just (CEq "1.0.0")
          , dependencyLocations = []
          , dependencyEnvironments = Set.fromList []
          , dependencyTags = Map.empty
          }
  MavenDependency dep (Set.fromList []) $ Set.fromList submodules

scopeFilters :: Spec
scopeFilters = do
  describe "Filter maven deps by scope" $ do
    it "should not filter from empty include set" $ do
      let compileDep = createDepFromScopes ["compile"]
      let testDep = createDepFromScopes ["test"]
      let graph = Graphing.directs [compileDep, testDep] <> Graphing.edges [(compileDep, testDep)]
      let graph' = filterMavenDependencyByScope (MavenScopeOnlyFilters mempty) graph

      expectDirect [compileDep, testDep] graph'
      expectDeps [compileDep, testDep] graph'
      expectEdges [(compileDep, testDep)] graph'

    it "should not filter from empty dep scope and empty include set" $ do
      let dep = createDepFromScopes []
      let graph = Graphing.directs [dep]
      let graph' = filterMavenDependencyByScope (MavenScopeOnlyFilters mempty) graph

      expectDirect [dep] graph'
      expectDeps [dep] graph'
      expectEdges [] graph'

    it "should filter from empty dep scope and non empty include set" $ do
      -- Dep()
      -- After filtering with includeSet = Set("compile")
      -- empty
      let dep = createDepFromScopes []
      let includeSet = Set.fromList ["compile"]
      let graph = Graphing.directs [dep]
      let graph' = filterMavenDependencyByScope (MavenScopeOnlyFilters includeSet) graph

      expectDirect [] graph'
      expectDeps [] graph'
      expectEdges [] graph'

  it "should filter deps with scope not in the include set" $ do
    -- Dep(compile) -> Dep(provided)
    -- After filtering with includeSet = Set("provided")
    -- Dep(provided)
    let compileDep = createDepFromScopes ["compile"]
    let providedDep = createDepFromScopes ["provided"]
    let graph = Graphing.directs [compileDep] <> Graphing.edges [(compileDep, providedDep)]
    let includeSet = Set.fromList ["provided"]
    let graph' = filterMavenDependencyByScope (MavenScopeOnlyFilters includeSet) graph

    expectDirect [providedDep] graph'
    expectDeps [providedDep] graph'
    expectEdges [] graph'

  it "should filter multi scope deps when dep scope is not fully contained in include set" $ do
    -- Dep(compile, provided)
    -- After filtering with includeSet = Set("compile")
    -- empty
    let compileProvidedDep = createDepFromScopes ["compile", "provided"]
    let graph = Graphing.directs [compileProvidedDep]
    let includeSet = Set.fromList ["compile"]
    let graph' = filterMavenDependencyByScope (MavenScopeOnlyFilters includeSet) graph

    expectDirect [] graph'
    expectDeps [] graph'
    expectEdges [] graph'

  it "should not filter deps with empty exclude set" $ do
    -- Dep(compile)
    -- After filtering with excludeSet []
    -- Dep(compile)
    let compileDep = createDepFromScopes ["compile"]
    let graph = Graphing.directs [compileDep]
    let graph' = filterMavenDependencyByScope (MavenScopeExcludeFilters mempty) graph

    expectDirect [compileDep] graph'
    expectDeps [compileDep] graph'
    expectEdges [] graph'

  it "should not filter deps with no scope and empty exclude set" $ do
    -- Dep()
    -- After filtering with excludeSet []
    -- Dep()
    let dep = createDepFromScopes []
    let graph = Graphing.directs [dep]
    let graph' = filterMavenDependencyByScope (MavenScopeExcludeFilters mempty) graph

    expectDirect [dep] graph'
    expectDeps [dep] graph'
    expectEdges [] graph'

  it "should not filter deps with no scope and non-empty exclude set" $ do
    -- Dep()
    -- After filtering with excludeSet ["compile"]
    -- Dep()
    let dep = createDepFromScopes []
    let graph = Graphing.directs [dep]
    let excludeSet = Set.fromList [MavenScopeFilterPredicateSingle "compile"]
    let graph' = filterMavenDependencyByScope (MavenScopeExcludeFilters excludeSet) graph

    expectDirect [dep] graph'
    expectDeps [dep] graph'
    expectEdges [] graph'

  it "should filter deps with scope in exclude set" $ do
    -- Dep(compile) -> Dep(provided)
    -- After filtering with excludeSet ["provided"]
    -- Dep(compile)
    let compileDep = createDepFromScopes ["compile"]
    let providedDep = createDepFromScopes ["provided"]
    let graph = Graphing.directs [compileDep] <> Graphing.edges [(compileDep, providedDep)]
    let excludeSet = Set.fromList [MavenScopeFilterPredicateSingle "provided"]
    let graph' = filterMavenDependencyByScope (MavenScopeExcludeFilters excludeSet) graph

    expectDirect [compileDep] graph'
    expectDeps [compileDep] graph'
    expectEdges [] graph'

  it "should filter deps with multi-scope in exclude set" $ do
    -- Dep(compile, provided) -> Dep(compile)
    --                               \
    --  Dep(runtime)    ->       Dep(provided)
    -- After filtering with excludeSet = Set("provided")
    --
    -- Dep(compile)
    -- Dep(runtime)
    let compileProvidedDep = createDepFromScopes ["compile", "provided"]
    let providedDep = createDepFromScopes ["provided"]
    let compileDep = createDepFromScopes ["compile"]
    let runtimeDep = createDepFromScopes ["runtime"]
    let graph = Graphing.directs [compileProvidedDep, runtimeDep] <> Graphing.edges [(compileProvidedDep, compileDep), (compileDep, providedDep), (runtimeDep, providedDep)]
    let excludeSet = Set.fromList [MavenScopeFilterPredicateSingle "provided"]
    let graph' = filterMavenDependencyByScope (MavenScopeExcludeFilters excludeSet) graph

    expectDirect [compileDep, runtimeDep] graph'
    expectDeps [runtimeDep, compileDep] graph'
    expectEdges [] graph'

  it "should filter deps with multi-scope and maintain edges" $ do
    -- Dep(compile) -> Dep(provided, test) -> Dep(runtime)
    --
    -- After filtering with includeSet = Set("compile", "runtime") & excludeSet = Set("provided", "test")
    --
    -- Dep(compile) -> Dep(runtime)
    let providedTestDep = createDepFromScopes ["provided", "test"]
    let compileDep = createDepFromScopes ["compile"]
    let runtimeDep = createDepFromScopes ["runtime"]
    let graph = Graphing.directs [compileDep] <> Graphing.edges [(compileDep, providedTestDep), (providedTestDep, runtimeDep)]
    let excludeSet = Set.fromList [MavenScopeFilterPredicateSingle "provided", MavenScopeFilterPredicateSingle "test"]
    let graph' = filterMavenDependencyByScope (MavenScopeExcludeFilters excludeSet) graph

    expectDirect [compileDep] graph'
    expectDeps [compileDep, runtimeDep] graph'
    expectEdges [(compileDep, runtimeDep)] graph'

  it "should filter deps with multi-scope only when the dep matches all scopes in the predicate" $ do
    let compileDep = createDepFromScopes ["compile"]
    let providedDep = createDepFromScopes ["provided"]
    let providedTestDep = createDepFromScopes ["provided", "test"]
    let graph = Graphing.directs [compileDep, providedTestDep] <> Graphing.edges [(compileDep, providedDep)]
    let excludeSet = Set.fromList [MavenScopeFilterPredicateCombined $ Set.fromList ["provided", "test"]]
    let graph' = filterMavenDependencyByScope (MavenScopeExcludeFilters excludeSet) graph

    expectDirect [compileDep] graph'
    expectDeps [compileDep, providedDep] graph'
    expectEdges [(compileDep, providedDep)] graph'

  it "should filter deps with multi-scope and single-scope predicates" $ do
    let compileDep = createDepFromScopes ["compile"]
    let providedDep = createDepFromScopes ["provided"]
    let providedTestDep = createDepFromScopes ["provided", "test"]
    let graph = Graphing.directs [compileDep, providedTestDep] <> Graphing.edges [(compileDep, providedDep)]
    let excludeSet = Set.fromList [MavenScopeFilterPredicateCombined $ Set.fromList ["provided", "test"], MavenScopeFilterPredicateSingle "compile"]
    let graph' = filterMavenDependencyByScope (MavenScopeExcludeFilters excludeSet) graph

    expectDirect [providedDep] graph'
    expectDeps [providedDep] graph'
    expectEdges [] graph'

submoduleDep1 :: MavenDependency
submoduleDep1 = createDepFromName "com.fossa:submodule1" []

submoduleDep2 :: MavenDependency
submoduleDep2 = createDepFromName "com.fossa:submodule2" []

sharedDep :: MavenDependency
sharedDep = createDepFromName "com.fossa:sharedDep" []

sharedDepTransitive :: MavenDependency
sharedDepTransitive = createDepFromName "com.fossa:sharedDepTransitive" []

filteredDep :: MavenDependency
filteredDep = createDepFromName "com.fossa:filteredDep" []

coloredSubmoduleDep1 :: MavenDependency
coloredSubmoduleDep1 = createDepFromName "com.fossa:submodule1" ["com.fossa:submodule1"]

coloredSharedDep :: MavenDependency
coloredSharedDep = createDepFromName "com.fossa:sharedDep" ["com.fossa:submodule1", "com.fossa:submodule2"]

coloredSharedDepTransitive :: MavenDependency
coloredSharedDepTransitive = createDepFromName "com.fossa:sharedDepTransitive" ["com.fossa:submodule1", "com.fossa:submodule2"]

includedSubmodules :: Set.Set Text
includedSubmodules = Set.fromList ["com.fossa:submodule1"]

allSubmodules :: Set.Set Text
allSubmodules = Set.fromList ["com.fossa:submodule1", "com.fossa:submodule2"]

filteredGraph :: Graphing.Graphing MavenDependency
filteredGraph = Graphing.directs [coloredSubmoduleDep1] <> Graphing.edges [(coloredSubmoduleDep1, coloredSharedDep), (coloredSharedDep, coloredSharedDepTransitive)]

submoduleFilters :: Spec
submoduleFilters = do
  describe "Filter maven deps by submodule" $ do
    it "should filter submodule but not its shared dependencies" $ do
      -- com.fossa:submodule2     ->   com.fossa:filteredDep
      --                         \
      -- com.fossa:submodule1 ->   com.fossa:sharedDep -> com.fossa:sharedDepTransitive
      --
      -- After filtering with includedSubmodules = Set("com.fossa:submodule1")
      --
      -- com.fossa:submodule1 ->  com.fossa:sharedDep -> com.fossa:sharedDepTransitive

      let graph = Graphing.directs [submoduleDep1, submoduleDep2] <> Graphing.edges [(submoduleDep1, sharedDep), (submoduleDep2, sharedDep), (submoduleDep2, filteredDep), (sharedDep, sharedDepTransitive)]

      filterMavenSubmodules includedSubmodules allSubmodules graph `shouldBe` filteredGraph

    it "should filter submodule when it is a dependency of another submodule and keep the shared dependencies" $ do
      -- com.fossa:submodule1                    ->      com.fossa:sharedDep -> com.fossa:sharedDepTransitive
      --                       \                       /
      --                         com.fossa:submodule2 ->  com.fossa:filteredDep -> com.fossa:filteredDepTransitive
      --
      -- After filtering with includedSubmodules = Set("com.fossa:submodule1")
      --
      -- com.fossa:submodule1 ->  com.fossa:sharedDep -> com.fossa:sharedDepTransitive
      let filteredDepTransitive = createDepFromName "com.fossa:filteredDepTransitive" []
      let graph = Graphing.directs [submoduleDep1, submoduleDep2] <> Graphing.edges [(submoduleDep1, sharedDep), (submoduleDep1, submoduleDep2), (submoduleDep2, sharedDep), (submoduleDep2, filteredDep), (sharedDep, sharedDepTransitive), (filteredDep, filteredDepTransitive)]

      filterMavenSubmodules includedSubmodules allSubmodules graph `shouldBe` filteredGraph

spec :: Spec
spec = do
  scopeFilters
  submoduleFilters
