{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Grapher is a thin State wrapper effect around 'G.Graphing'
--
-- It defines @direct@ and @edge@ combinators analagous to 'G.direct' and
-- 'G.edge' from 'G.Graphing'
module Effect.Grapher
  ( Grapher (..),
    direct,
    edge,
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
    MappingError(..),

    -- * Re-exports
    module X,
  )
where

import Control.Algebra as X
import Control.Carrier.Diagnostics (ToDiagnostic(..))
import Control.Carrier.State.Strict
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (pretty)
import qualified Graphing as G

data Grapher ty (m :: Type -> Type) k where
  Direct :: ty -> Grapher ty m ()
  Edge :: ty -> ty -> Grapher ty m ()

direct :: Has (Grapher ty) sig m => ty -> m ()
direct ty = send (Direct ty)

edge :: Has (Grapher ty) sig m => ty -> ty -> m ()
edge parent child = send (Edge parent child)

runGrapher :: GrapherC ty m a -> m (G.Graphing ty, a)
runGrapher = runState G.empty . runGrapherC

evalGrapher :: Functor m => GrapherC ty m a -> m (G.Graphing ty)
evalGrapher = execState G.empty . runGrapherC

newtype GrapherC ty m a = GrapherC {runGrapherC :: StateC (G.Graphing ty) m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Ord ty) => Algebra (Grapher ty :+: sig) (GrapherC ty m) where
  alg hdl sig ctx = GrapherC $ case sig of
    L (Direct ty) -> modify (G.direct ty) *> pure ctx
    L (Edge parent child) -> modify (G.edge parent child) *> pure ctx
    R other -> alg (runGrapherC . hdl) (R other) ctx

----- Labeling

-- | An extension of 'Grapher' that tracks a set of associated /labels/ for
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
insertLabel ty lbl = M.insertWith (<>) ty (S.singleton lbl)

-- | An interpreter for @LabeledGrapher@. See existing strategies for examples
withLabeling :: (Monad m, Ord ty, Ord res) => (ty -> Set lbl -> res) -> LabeledGrapherC ty lbl m a -> m (G.Graphing res)
withLabeling f act = do
  (graph, (labels, _)) <- runGrapher . runState M.empty $ act
  pure (unlabel f labels graph)

-- | Transform a @Graphing ty@ into a @Graphing ty'@, given a function that
-- transforms the old node type @ty@ and a set of labels on that node
unlabel :: (Ord ty, Ord ty') => (ty -> Set lbl -> ty') -> Labels ty lbl -> G.Graphing ty -> G.Graphing ty'
unlabel f labels = G.gmap (\ty -> f ty (findLabels ty))
  where
    findLabels ty = fromMaybe S.empty (M.lookup ty labels)

----- Mapping

-- | An extension of 'Grapher' that tracks an associated value for each node in
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
mapping ty val = modify (M.insert ty val)

withMapping :: (Monad m, Ord ty, Ord res, Show ty) => (ty -> val -> res) -> MappedGrapherC ty val m a -> m (Either MappingError (G.Graphing res))
withMapping f act = do
  (graph, (labels, _)) <- runGrapher . runState M.empty $ act
  let -- result :: Either MappingError (G.Graphing res)
      result = G.gtraverse replaceVal graph

      -- replaceVal :: ty -> Either MappingError res
      replaceVal key =
        case M.lookup key labels of
          Nothing -> Left . MissingKey . T.pack $ show key
          Just value -> Right $ f key value

  pure result

-- | Errors that may occur when using 'withMapping'
newtype MappingError = MissingKey Text -- ^ A key did not have an associated value
  deriving (Eq, Ord, Show)

instance ToDiagnostic MappingError where
  renderDiagnostic (MissingKey key) = "Missing associated value for key: " <> pretty key
