module Maven.CommonSpec (
  spec,
) where

import Data.Text (Text)
import DepTypes (DepType (MavenType), Dependency (..), VerConstraint (CEq))

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Discovery.Filters (MavenScopeFilters (MavenScopeExcludeFilters, MavenScopeIncludeFilters), setExclude, setInclude)
import GraphUtil
import Graphing qualified
import Strategy.Maven.Common (MavenDependency (..), filterMavenDependencyByScope)
import Test.Hspec (Spec, describe, it)

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
  MavenDependency dep (Set.fromList scopes)

spec :: Spec
spec = do
  describe "Filter maven deps by scope" $ do
    it "should not filter from empty include set" $ do
      let compileDep = createDepFromScopes ["compile"]
      let testDep = createDepFromScopes ["test"]
      let graph = Graphing.directs [compileDep, testDep] <> Graphing.edges [(compileDep, testDep)]
      let graph' = filterMavenDependencyByScope (MavenScopeIncludeFilters $ setInclude $ Set.fromList []) graph

      expectDirect [compileDep, testDep] graph'
      expectDeps [compileDep, testDep] graph'
      expectEdges [(compileDep, testDep)] graph'

    it "should not filter from empty dep scope and empty include set" $ do
      let dep = createDepFromScopes []
      let graph = Graphing.directs [dep]
      let graph' = filterMavenDependencyByScope (MavenScopeIncludeFilters $ setInclude $ Set.fromList []) graph

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
      let graph' = filterMavenDependencyByScope (MavenScopeIncludeFilters $ setInclude includeSet) graph

      expectDirect [] graph'
      expectDeps [] graph'
      expectEdges [] graph'

  it "should filter deps with scope not in the include set" $ do
    -- Dep(compile) -> Dep(provided)
    -- After filtering with includeSet = Set"provided")
    -- Dep(provided)
    let compileDep = createDepFromScopes ["compile"]
    let providedDep = createDepFromScopes ["provided"]
    let graph = Graphing.directs [compileDep] <> Graphing.edges [(compileDep, providedDep)]
    let includeSet = Set.fromList ["provided"]
    let graph' = filterMavenDependencyByScope (MavenScopeIncludeFilters $ setInclude includeSet) graph

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
    let graph' = filterMavenDependencyByScope (MavenScopeIncludeFilters $ setInclude includeSet) graph

    expectDirect [] graph'
    expectDeps [] graph'
    expectEdges [] graph'

  it "should not filter deps with empty exclude set" $ do
    -- Dep(compile)
    -- After filtering with excludeSet []
    -- Dep(compile)
    let compileDep = createDepFromScopes ["compile"]
    let graph = Graphing.directs [compileDep]
    let graph' = filterMavenDependencyByScope (MavenScopeExcludeFilters $ setExclude $ Set.fromList []) graph

    expectDirect [compileDep] graph'
    expectDeps [compileDep] graph'
    expectEdges [] graph'

  it "should not filter deps with no scope and empty exclude set" $ do
    -- Dep()
    -- After filtering with excludeSet []
    -- Dep()
    let dep = createDepFromScopes []
    let graph = Graphing.directs [dep]
    let graph' = filterMavenDependencyByScope (MavenScopeExcludeFilters $ setExclude $ Set.fromList []) graph

    expectDirect [dep] graph'
    expectDeps [dep] graph'
    expectEdges [] graph'

  it "should not filter deps with no scope and non-empty exclude set" $ do
    -- Dep()
    -- After filtering with excludeSet ["compile"]
    -- Dep()
    let dep = createDepFromScopes []
    let graph = Graphing.directs [dep]
    let excludeSet = Set.fromList ["compile"]
    let graph' = filterMavenDependencyByScope (MavenScopeExcludeFilters $ setExclude excludeSet) graph

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
    let excludeSet = Set.fromList ["provided"]
    let graph' = filterMavenDependencyByScope (MavenScopeExcludeFilters $ setExclude excludeSet) graph

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
    let excludeSet = Set.fromList ["provided"]
    let graph' = filterMavenDependencyByScope (MavenScopeExcludeFilters $ setExclude excludeSet) graph

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
    let excludeSet = Set.fromList ["provided", "test"]
    let graph' = filterMavenDependencyByScope (MavenScopeExcludeFilters $ setExclude excludeSet) graph

    expectDirect [compileDep] graph'
    expectDeps [compileDep, runtimeDep] graph'
    expectEdges [(compileDep, runtimeDep)] graph'
