{-# language TemplateHaskell #-}

module Strategy.Node.YarnLock
  ( analyze'
  , buildGraph
  ) where

import Control.Effect.Diagnostics
import Data.Bifunctor (first)
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.MultiKeyedMap as MKM
import DepTypes
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Path
import qualified Yarn.Lock as YL
import qualified Yarn.Lock.Types as YL

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' lockfile = do
  let path = fromAbsFile lockfile

  contents <- readContentsText lockfile
  case YL.parse path contents of
    Left err -> fatal (FileParseError path (YL.prettyLockfileError err))
    Right parsed -> pure (buildGraph parsed)

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
                     YL.ScopedPackageKey scope name -> "@" <> scope <> "/" <> name
               , dependencyVersion = Just (CEq (YL.version package))
               , dependencyLocations =
                   case YL.remote package of
                     YL.FileLocal _ _ -> [] -- FUTURE: upload this for analysis?
                     YL.FileLocalNoIntegrity _ -> [] -- FUTURE: upload this for analysis?
                     YL.FileRemote url _ -> [url]
                     YL.FileRemoteNoIntegrity url -> [url]
                     YL.GitRemote url rev -> [url <> "@" <> rev]
               , dependencyEnvironments = []
               , dependencyTags = M.empty
               }
