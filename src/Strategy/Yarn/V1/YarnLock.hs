module Strategy.Yarn.V1.YarnLock (
  analyze,
  buildGraph,
) where

import Control.Effect.Diagnostics
import Data.Bifunctor (first)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.MultiKeyedMap qualified as MKM
import DepTypes
import Effect.ReadFS
import Graphing (Graphing)
import Graphing qualified
import Path
import Yarn.Lock qualified as YL
import Yarn.Lock.Types qualified as YL

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze lockfile = context "Lockfile V1 analysis" $ do
  let path = fromAbsFile lockfile

  contents <- readContentsText lockfile
  case YL.parse path contents of
    Left err -> fatal (FileParseError path (YL.prettyLockfileError err))
    Right parsed -> context "Building dependency graph" $ pure $ buildGraph parsed

buildGraph :: YL.Lockfile -> Graphing Dependency
buildGraph lockfile =
  Graphing.edges (concatMap catMaybes (edgesForPackage <$> packagesList))
    <> Graphing.deeps (map toDependency packageWithoutOutEdges)
  where
    packagesList :: [(YL.PackageKey, YL.Package)]
    packagesList = map (first NE.head) $ MKM.toList lockfile

    packageWithoutOutEdges :: [(YL.PackageKey, YL.Package)]
    packageWithoutOutEdges = filter isWithoutAnyReachableDeps packagesList

    isWithoutAnyReachableDeps :: (a, YL.Package) -> Bool
    isWithoutAnyReachableDeps pkg = not (any isMember $ depsOf pkg) || null (depsOf pkg)

    isMember :: YL.PackageKey -> Bool
    isMember = flip MKM.member lockfile

    depsOf :: (a, YL.Package) -> [YL.PackageKey]
    depsOf = YL.dependencies . snd

    edgesForPackage :: (YL.PackageKey, YL.Package) -> [Maybe (Dependency, Dependency)]
    edgesForPackage parentPkg@(_, package) = do
      childKey <- YL.dependencies package

      let childPackageAtKey = MKM.lookup childKey lockfile
      case childPackageAtKey of
        Nothing -> pure Nothing
        Just childPackage ->
          pure $ Just (toDependency parentPkg, toDependency (childKey, childPackage))

    toDependency :: (YL.PackageKey, YL.Package) -> Dependency
    toDependency (key, package) =
      Dependency
        { dependencyType = NodeJSType
        , dependencyName =
            case YL.name key of
              YL.SimplePackageKey name -> name
              YL.ScopedPackageKey scope name -> "@" <> scope <> "/" <> name
        , dependencyVersion = Just (CEq (YL.version package))
        , dependencyLocations =
            case YL.remote package of
              YL.FileLocal _ _ -> [] -- FUTURE: upload this for analysis?
              YL.FileLocalNoIntegrity _ -> [] -- FUTURE: upload this for analysis?
              YL.FileRemote url _ -> [url]
              YL.FileRemoteNoIntegrity url -> [url]
              YL.GitRemote url rev -> [url <> "@" <> rev]
              YL.DirectoryLocal dirPath -> [dirPath]
              YL.DirectoryLocalSymLinked dirPath -> [dirPath]
        , dependencyEnvironments = []
        , dependencyTags = Map.empty
        }
