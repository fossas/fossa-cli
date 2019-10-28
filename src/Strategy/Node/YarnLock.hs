module Strategy.Node.YarnLock
  ( discover
  , strategy
  , analyze
  , configure
  ) where

import Prologue hiding (many, some)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.MultiKeyedMap as MKM
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.State
import qualified Yarn.Lock as YL
import qualified Yarn.Lock.Types as YL

import           Diagnostics
import           Discovery.Walk
import           Effect.GraphBuilder
import           Effect.ReadFS
import qualified Graph as G
import           Types

discover :: Discover
discover = Discover
  { discoverName = "yarn-lock"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ subdirs files -> do
  for_ files $ \f ->
    when (fileName f == "yarn.lock") $
      output (configure f)

  walkSkipNamed ["node_modules/"] subdirs

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "nodejs-yarnlock"
  , strategyAnalyze = analyze . targetFile
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

analyze :: Members '[Error CLIErr, ReadFS] r => Path Rel File -> Sem r G.Graph
analyze lockfile = do
    let path = fromRelFile lockfile

    contents <- readContentsText lockfile
    case YL.parse path contents of
      Left err -> throw (FileParseError path (YL.prettyLockfileError err))
      Right a -> pure (buildGraph a)

buildGraph :: YL.Lockfile -> G.Graph
buildGraph lockfile = run . evalGraphBuilder G.empty . runState @(Map YL.PackageKey G.DepRef) M.empty $
  traverse add (MKM.toList lockfile)
  where
  add :: Members '[State (Map YL.PackageKey G.DepRef), GraphBuilder] r => (NE.NonEmpty YL.PackageKey, YL.Package) -> Sem r G.DepRef
  add (keys, package) = do
    seen <- get
    case asum (map (`M.lookup` seen) (NE.toList keys)) of
      Just ref -> pure ref
      Nothing -> do
        ref <- addNode (toDependency (NE.head keys) package)
        modify (\seen' -> foldr (`M.insert` ref) seen' (NE.toList keys))
        children <- traverse (add . (\k -> (k NE.:| [], MKM.at lockfile k))) (YL.dependencies package)
        traverse_ (addEdge ref) children
        pure ref

  toDependency key package =
    G.Dependency { dependencyType = G.NodeJSType
                 , dependencyName =
                     case YL.name key of
                       YL.SimplePackageKey name -> name
                       YL.ScopedPackageKey scope name -> scope <> "/" <> name
                 , dependencyVersion = Just (G.CEq (YL.version package))
                 , dependencyLocations =
                     case YL.remote package of
                       YL.FileLocal _ _ -> [] -- FUTURE: upload this for analysis?
                       YL.FileRemote url _ -> [url]
                       YL.GitRemote url rev -> [url <> "@" <> rev]
                 , dependencyTags = M.empty
                 }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
