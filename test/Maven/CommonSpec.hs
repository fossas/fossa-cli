module Maven.CommonSpec (
  spec,
) where

import Data.Text (Text)
import DepTypes (DepType (MavenType), Dependency (..), VerConstraint (CEq))

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
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
    it "should not filter from empty include and exclude sets" $ do
      let compileDep = createDepFromScopes ["compile"]
      let testDep = createDepFromScopes ["test"]
      let graph = Graphing.directs [compileDep, testDep] <> Graphing.edges [(compileDep, testDep)]
      let graph' = filterMavenDependencyByScope (Set.fromList []) (Set.fromList []) graph

      expectDirect [compileDep, testDep] graph'
      expectDeps [compileDep, testDep] graph'
      expectEdges [(compileDep, testDep)] graph'

    it "should not filter from empty dep scope and non empty include and exclude sets" $ do
      -- Dep()
      -- After filtering with includeSet = Set("compile") and excludeSet = Set("test")
      -- Dep()
      let dep = createDepFromScopes []
      let includeSet = Set.fromList ["compile"]
      let excludeSet = Set.fromList ["test"]
      let graph = Graphing.directs [dep]
      let graph' = filterMavenDependencyByScope includeSet excludeSet graph

      expectDirect [dep] graph'
      expectDeps [dep] graph'
      expectEdges [] graph'

    it "should filter deps with scope not in the include set when exclude set is empty" $ do
      -- Dep(compile) -> Dep(provided)
      -- After filtering with includeSet = Set"provided")
      -- Dep(provided)
      let compileDep = createDepFromScopes ["compile"]
      let providedDep = createDepFromScopes ["provided"]
      let graph = Graphing.directs [compileDep, providedDep] <> Graphing.edges [(compileDep, providedDep)]
      let includeSet = Set.fromList ["provided"]
      let graph' = filterMavenDependencyByScope includeSet (Set.fromList []) graph

      expectDirect [providedDep] graph'
      expectDeps [providedDep] graph'
      expectEdges [] graph'

    it "should filter multi scope deps not in include set when exclude set is empty" $ do
      -- Dep(compile, provided)
      -- After filtering with includeSet = Set"runtime")
      -- empty
      let compileProvidedDep = createDepFromScopes ["compile", "provided"]
      let graph = Graphing.directs [compileProvidedDep]
      let includeSet = Set.fromList ["runtime"]
      let graph' = filterMavenDependencyByScope includeSet (Set.fromList []) graph

      expectDirect [] graph'
      expectDeps [] graph'
      expectEdges [] graph'

    it "should filter deps with scope in exclude set when include set is empty" $ do
      -- Dep(compile) -> Dep(provided)
      -- After filtering with excludeSet ["provided"]
      -- Dep(compile)
      let compileDep = createDepFromScopes ["compile"]
      let providedDep = createDepFromScopes ["provided"]
      let graph = Graphing.directs [compileDep, providedDep] <> Graphing.edges [(compileDep, providedDep)]
      let excludeSet = Set.fromList ["provided"]
      let graph' = filterMavenDependencyByScope (Set.fromList []) excludeSet graph

      expectDirect [compileDep] graph'
      expectDeps [compileDep] graph'
      expectEdges [] graph'

    it "should filter deps with scope not in the include set and in exclude set" $ do
      -- Dep(compile) -> Dep(provided)
      -- After filtering with includeSet = Set("runtime") & excludeSet = Set("compile")
      -- empty
      let compileDep = createDepFromScopes ["compile"]
      let providedDep = createDepFromScopes ["provided"]
      let graph = Graphing.directs [compileDep, providedDep] <> Graphing.edges [(compileDep, providedDep)]
      let includeSet = Set.fromList ["runtime"]
      let excludeSet = Set.fromList ["compile"]
      let graph' = filterMavenDependencyByScope includeSet excludeSet graph

      expectDirect [] graph'
      expectDeps [] graph'
      expectEdges [] graph'

    it "should filter when scope is in both include and exclude set" $ do
      -- Dep(provided)
      -- After filtering with includeSet = Set("provided") & excludeSet = Set("provided")
      -- empty graph
      let providedDep = createDepFromScopes ["provided"]
      let graph = Graphing.directs [providedDep]
      let includeSet = Set.fromList ["provided"]
      let excludeSet = Set.fromList ["provided"]
      let graph' = filterMavenDependencyByScope includeSet excludeSet graph

      expectDirect [] graph'
      expectDeps [] graph'
      expectEdges [] graph'

    it "should filter deps with multi-scope" $ do
      -- Dep(compile, provided) -> Dep(compile)
      --                               \
      --  Dep(runtime)    ->       Dep(provided)
      -- After filtering with includeSet = Set("compile", "runtime") & excludeSet = Set("provided")
      --
      -- Dep(compile, provided) -> Dep(compile)
      -- Dep(runtime)
      let compileProvidedDep = createDepFromScopes ["compile", "provided"]
      let providedDep = createDepFromScopes ["provided"]
      let compileDep = createDepFromScopes ["compile"]
      let runtimeDep = createDepFromScopes ["runtime"]
      let graph = Graphing.directs [compileProvidedDep, runtimeDep] <> Graphing.edges [(compileProvidedDep, compileDep), (compileDep, providedDep), (runtimeDep, providedDep)]
      let includeSet = Set.fromList ["compile", "runtime"]
      let excludeSet = Set.fromList ["provided"]
      let graph' = filterMavenDependencyByScope includeSet excludeSet graph

      expectDirect [compileProvidedDep, runtimeDep] graph'
      expectDeps [compileProvidedDep, runtimeDep, compileDep] graph'
      expectEdges [(compileProvidedDep, compileDep)] graph'

    it "should filter deps with multi-scope and maintain edges" $ do
      -- Dep(compile) -> Dep(provided, test) -> Dep(runtime)
      --
      -- After filtering with includeSet = Set("compile", "runtime") & excludeSet = Set("provided")
      --
      -- Dep(compile) -> Dep(runtime)
      let providedTestDep = createDepFromScopes ["provided", "test"]
      let compileDep = createDepFromScopes ["compile"]
      let runtimeDep = createDepFromScopes ["runtime"]
      let graph = Graphing.directs [compileDep] <> Graphing.edges [(compileDep, providedTestDep), (providedTestDep, runtimeDep)]
      let includeSet = Set.fromList ["compile", "runtime"]
      let excludeSet = Set.fromList ["provided"]
      let graph' = filterMavenDependencyByScope includeSet excludeSet graph

      expectDirect [compileDep] graph'
      expectDeps [compileDep, runtimeDep] graph'
      expectEdges [(compileDep, runtimeDep)] graph'
