{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Grapher is a thin State wrapper effect around 'G.Graphing'
--
-- It defines @direct@ and @edge@ combinators analagous to 'G.direct' and
-- 'G.edge' from 'G.Graphing'
module Effect.Grapher (
  Grapher,
  SGrapher (..),
  GrapherC,
  direct,
  edge,
  edges,
  deep,
  evalGrapher,
  runGrapher,

  -- * Labeling
  LabeledGrapher,
  LabeledGrapherC,
  label,
  withLabeling,

  -- * Mapping
  MappedGrapher,
  MappedGrapherC,
  mapping,
  withMapping,
  MappingError (..),

  -- * Re-exports
  module X,
) where

import Control.Algebra as X
import Control.Carrier.Diagnostics (ToDiagnostic (..))
import Control.Carrier.Simple
import Control.Carrier.State.Strict
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text)
import Graphing qualified as G
import Prettyprinter (pretty)

data SGrapher ty k where
  Direct :: ty -> SGrapher ty ()
  Edge :: ty -> ty -> SGrapher ty ()
  Deep :: ty -> SGrapher ty ()

type Grapher ty = Simple (SGrapher ty)

direct :: Has (Grapher ty) sig m => ty -> m ()
direct = sendSimple . Direct

edge :: Has (Grapher ty) sig m => ty -> ty -> m ()
edge parent child = sendSimple (Edge parent child)

edges :: Traversable t => Has (Grapher ty) sig m => t (ty, ty) -> m ()
edges = traverse_ (uncurry edge)

deep :: Has (Grapher ty) sig m => ty -> m ()
deep = sendSimple . Deep

evalGrapher :: (Ord ty, Algebra sig m) => GrapherC ty m a -> m (G.Graphing ty)
evalGrapher = fmap fst . runGrapher

type GrapherC ty m = SimpleStateC (G.Graphing ty) (SGrapher ty) m

runGrapher :: (Ord ty, Algebra sig m) => GrapherC ty m a -> m (G.Graphing ty, a)
runGrapher = interpretState G.empty $ \case
  Direct ty -> modify (G.direct ty <>)
  Edge parent child -> modify (G.edge parent child <>)
  Deep n -> modify (G.deep n <>)

----- Labeling

-- | An extension of 'SGrapher that tracks a set of associated /labels/ for
-- each node in the graph.
--
-- For example, given a node type of:
--
-- > data PipPkg = PipPkg { pipPkgName :: Text, pipPkgVersion :: Text }
-- >   deriving (Eq, Ord, Show)
--
-- We also want to keep track of additional metadata we encounter (or "labels"):
--
-- > data PipLabel =
-- >     PipLocation Text
-- >   | PipEnvironment Text
-- >   deriving (Eq, Ord, Show)
--
-- We eventually use these labels to construct the final @Dependency@s in our
-- graph.
--
-- We create a type synonym:
--
-- > type PipGrapher = LabeledGrapher PipPkg PipLabel
--
-- and now we can use our newly-minted "PipGrapher" effect when describing our
-- dependency graph:
--
-- > doTheThing :: Has PipGrapher sig m => ...
--
-- This allows us to use 'direct' and 'edge' like before, but also gives us a
-- third primitive, 'label'.
--
-- > label :: Has PipGrapher sig m => PipPkg -> PipLabel -> m ()
type LabeledGrapher ty lbl = State (Labels ty lbl) :+: Grapher ty

type Labels ty lbl = Map ty (Set lbl)

type LabeledGrapherC ty lbl m a = StateC (Labels ty lbl) (GrapherC ty m) a

label :: (Ord ty, Ord lbl, Has (LabeledGrapher ty lbl) sig m) => ty -> lbl -> m ()
label ty lbl = modify (insertLabel ty lbl)

insertLabel :: (Ord ty, Ord lbl) => ty -> lbl -> Labels ty lbl -> Labels ty lbl
insertLabel ty lbl = Map.insertWith (<>) ty (Set.singleton lbl)

-- | An interpreter for @LabeledGrapher@. See existing strategies for examples
withLabeling :: (Algebra sig m, Ord ty, Ord res) => (ty -> Set lbl -> res) -> LabeledGrapherC ty lbl m a -> m (G.Graphing res)
withLabeling f act = do
  (graph, (labels, _)) <- runGrapher . runState Map.empty $ act
  pure (unlabel f labels graph)

-- | Transform a @Graphing ty@ into a @Graphing ty'@, given a function that
-- transforms the old node type @ty@ and a set of labels on that node
unlabel :: (Ord ty, Ord ty') => (ty -> Set lbl -> ty') -> Labels ty lbl -> G.Graphing ty -> G.Graphing ty'
unlabel f labels = G.gmap (\ty -> f ty (findLabels ty))
  where
    findLabels ty = fromMaybe Set.empty (Map.lookup ty labels)

----- Mapping

-- | An extension of 'SGrapher that tracks an associated value for each node in
-- the graph. It is expected that each node in the graph /must/ have an
-- associated value.
--
-- For example, cabal.plan uses opaque @UnitID@s to reference packages:
--
-- > newtype UnitId = UnitId { unUnitId :: Text }
-- >   deriving (Eq, Ord, Show)
--
-- This is the type we use to build edges and mark direct dependencies.
--
-- We also want to associate package contents with a UnitId
--
-- > data CabalPackage = CabalPackage
-- >   { cabalPackageName :: Text
-- >   , cabalPackageVersion :: Text
-- >   , ...
-- >   }
--
-- We can eventually use these associations to construct the final nodes in our
-- graph.
--
-- One way to use this, forcing better type inference, is to create a type
-- synonym:
--
-- > type CabalGrapher = MappedGrapher UnitId HaskellPackage
--
-- and now we can use our newly-minted "CabalGrapher" effect when describing our
-- dependency graph:
--
-- > doTheThing :: Has CabalGrapher sig m => ...
--
-- This allows us to use 'direct' and 'edge' like before, but also gives us a
-- third primitive, 'mapping'.
--
-- > mapping :: Has CabalGrapher sig m => UnitId -> CabalPackage -> m ()
type MappedGrapher ty val = State (Map ty val) :+: Grapher ty

type MappedGrapherC ty val m a = StateC (Map ty val) (GrapherC ty m) a

-- | Associate a key with a given value
mapping :: (Ord ty, Has (MappedGrapher ty val) sig m) => ty -> val -> m ()
mapping ty val = modify (Map.insert ty val)

withMapping :: (Algebra sig m, Ord ty, Ord res, Show ty) => (ty -> val -> res) -> MappedGrapherC ty val m a -> m (Either MappingError (G.Graphing res))
withMapping f act = do
  (graph, (labels, _)) <- runGrapher . runState Map.empty $ act
  let -- result :: Either MappingError (G.Graphing res)
      result = G.gtraverse replaceVal graph

      -- replaceVal :: ty -> Either MappingError res
      replaceVal key =
        case Map.lookup key labels of
          Nothing -> Left . MissingKey . toText $ show key
          Just value -> Right $ f key value

  pure result

-- | Errors that may occur when using 'withMapping'
newtype MappingError
  = -- | A key did not have an associated value
    MissingKey Text
  deriving (Eq, Ord, Show)

instance ToDiagnostic MappingError where
  renderDiagnostic (MissingKey key) = "Missing associated value for key: " <> pretty key
