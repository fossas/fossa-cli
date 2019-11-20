{-# language TemplateHaskell #-}

-- | A higher-level, declarative interface for building dependency graphs.
--
-- @Graphing ty@ has three primitives:
--
-- - 'direct', which adds a direct dependency
-- - 'edge', which adds an edge between two dependencies
-- - 'label', which labels a dependency with additional metadata. See "A datatype for labels" below
--
-- To use this interface, you need three things:
--
-- 1. __A uniquely-identifying datatype__ for packages in the context of your buildtool
-- 2. __A datatype for labels__ -- everything else that is not uniquely-identifying
-- 3. __A function to build 'Dependency's__, for use with 'graphingToGraph' or 'runGraphingPure'
--
-- == A uniquely-identifying datatype
--
-- The @Graphing ty@ effect uses the semantics of @Eq ty@ and @Ord ty@ when merging
-- and labeling dependency references in the graph. @ty@ should be uniquely-identifying
-- in the context of the buildtool. Labels and edges for "equal" vertices are merged
--
-- For example: in golang analyzers, the package import path is considered to be
-- uniquely-identifying.
--
-- > newtype GolangPackage = GolangPackage { goImportPath :: Text }
--
-- == A datatype for labels
--
-- Because we're still interested in, e.g., locations, version constraints, and
-- other package tags, we need a way to capture those details, or "labels". We
-- have a type family, 'PkgLabel', that encodes this behavior:
--
-- > type instance PkgLabel GolangPackage = GolangLabel
-- >
-- > data GolangLabel = GolangLabelVersion Text | GolangLabelLocation Text
--
-- == A function to build 'Dependency's
--
-- Now that we have a key type @ty@, and a label type @PkgLabel ty@, we just
-- need a function @ty -> Set (PkgLabel ty) -> Dependency@. This is usually a
-- simple fold over the labels.
--
-- This function can be passed to 'graphingToGraph' or 'runGraphing'
--
-- == Reachable dependencies
--
-- Unreachable dependencies -- i.e. dependencies that aren't direct or reachable
-- from direct deps -- will not be included in the resulting graph with 'graphingToGraph'.
-- This makes graph building vastly simpler: for example, when we fill the transitive
-- dependencies with @go list@, which often contains a ton of dependencies irrelevant
-- to our project, we can naively parse all of them into the graph. Those unreachable
-- dpeendencies won't be included.
module Effect.Graphing
  ( Graphing(..)
  , PkgLabel
  , direct
  , edge
  , label

  , graphingToGraph
  , runGraphing
  ) where

import Prologue hiding (empty, parent)

import           Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AM
import           Algebra.Graph.ToGraph (dfs)
import           Data.Kind (Type)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Polysemy
import           Polysemy.State

import           Effect.GraphBuilder
import qualified Graph as G

data Graphing ty m a where
  Direct :: ty -> Graphing ty m ()
  Edge :: ty -> ty -> Graphing ty m ()
  Label :: ty -> PkgLabel ty -> Graphing ty m ()

type family PkgLabel (pkg :: Type) :: Type

makeSem ''Graphing

graphingToGraph :: forall ty lbl r a . (lbl ~ PkgLabel ty, Ord ty, Ord lbl) => (ty -> Set lbl -> G.Dependency) -> Sem (Graphing ty ': r) a -> Sem r G.Graph
graphingToGraph toDependency act = evalGraphBuilder G.empty $ do
  (amap, lbls, directs, _) <- runGraphing (raiseUnder act)

  let depAmap = AM.gmap mkDependency amap
      depDirect = fmap mkDependency (S.toList directs)

      mkDependency :: ty -> G.Dependency
      mkDependency a = toDependency a (fromMaybe S.empty (M.lookup a lbls))

      nodes = dfs depDirect depAmap

  refs <- M.fromList <$> traverse addingNode nodes

  traverse_ (visitNode refs depAmap) nodes

  traverse_ (\dep -> traverse_ addDirect (M.lookup dep refs)) depDirect

  where

  -- add a node with GraphBuilder
  addingNode :: Member GraphBuilder r' => G.Dependency -> Sem r' (G.Dependency, G.DepRef)
  addingNode k = do
    ref <- addNode k
    pure (k, ref)

  -- visit a node, adding edges between it and all of its dependencies
  visitNode :: Member GraphBuilder r' => Map G.Dependency G.DepRef -> AdjacencyMap G.Dependency -> G.Dependency -> Sem r' ()
  visitNode refs amap node = traverse_ (visitEdge refs node) (S.toList $ AM.postSet node amap)

  -- visit an edge by adding it to the graph
  visitEdge :: Member GraphBuilder r' => Map G.Dependency G.DepRef -> G.Dependency -> G.Dependency -> Sem r' ()
  visitEdge refs parent child = do
    let edgeRefs = do
          parentRef <- M.lookup parent refs
          childRef <- M.lookup child refs
          pure (parentRef, childRef)

    traverse_ (uncurry addEdge) edgeRefs

runGraphing :: forall ty lbl r a. (lbl ~ PkgLabel ty, Ord ty, Ord lbl) => Sem (Graphing ty ': r) a -> Sem r (AdjacencyMap ty, Map ty (Set lbl), Set ty, a)
runGraphing = fmap (\(amap, (lbls, (directs, a))) -> (amap, lbls, directs, a))
                . runState @(AdjacencyMap ty) AM.empty
                . runState @(Map ty (Set lbl)) M.empty
                . runState @(Set ty) S.empty
                . reinterpret3 (\case
  Direct v -> modify (S.insert v) *> modify (AM.overlay (AM.vertex v))
  Edge v1 v2 -> modify (AM.overlay (AM.edge v1 v2))
  Label v lbl -> modify (M.insertWith (<>) v (S.singleton lbl)))
