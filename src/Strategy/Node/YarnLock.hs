module Strategy.Node.YarnLock
  ( discover
  , analyze
  ) where

import Prologue

import Control.Carrier.Error.Either
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.MultiKeyedMap as MKM
import qualified Yarn.Lock as YL
import qualified Yarn.Lock.Types as YL

import DepTypes
import Discovery.Walk
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ subdirs files -> do
  case find (\f -> fileName f == "yarn.lock") files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "nodejs-yarnlock" NodejsGroup $ analyze file

  walkSkipNamed ["node_modules/"] subdirs

analyze :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Rel File -> m ProjectClosure
analyze lockfile = do
  let path = fromRelFile lockfile

  contents <- readContentsText lockfile
  case YL.parse path contents of
    Left err -> throwError (FileParseError path (YL.prettyLockfileError err))
    Right a -> pure (mkProjectClosure lockfile a)

mkProjectClosure :: Path Rel File -> YL.Lockfile -> ProjectClosure
mkProjectClosure file lock = ProjectClosure
  { closureStrategyGroup = NodejsGroup
  , closureStrategyName  = "nodejs-yarnlock"
  , closureModuleDir     = parent file
  , closureDependencies  = dependencies
  , closureLicenses      = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph lock
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = Complete
    }

buildGraph :: YL.Lockfile -> Graphing Dependency
buildGraph lockfile = run . evalGrapher $
  traverse (add . first NE.head) (MKM.toList lockfile)
  where
  add :: Has (Grapher Dependency) sig m => (YL.PackageKey, YL.Package) -> m ()
  add parentPkg@(_, package) =
    for_ (YL.dependencies package) $ \childKey -> do
      let childPkg = (childKey, MKM.at lockfile childKey)
      edge (toDependency parentPkg) (toDependency childPkg)

  toDependency (key,package) =
    Dependency { dependencyType = NodeJSType
               , dependencyName =
                   case YL.name key of
                     YL.SimplePackageKey name -> name
                     YL.ScopedPackageKey scope name -> scope <> "/" <> name
               , dependencyVersion = Just (CEq (YL.version package))
               , dependencyLocations =
                   case YL.remote package of
                     YL.FileLocal _ _ -> [] -- FUTURE: upload this for analysis?
                     YL.FileLocalNoIntegrity _ -> [] -- FUTURE: upload this for analysis?
                     YL.FileRemote url _ -> [url]
                     YL.FileRemoteNoIntegrity url -> [url]
                     YL.GitRemote url rev -> [url <> "@" <> rev]
               , dependencyTags = M.empty
               }
