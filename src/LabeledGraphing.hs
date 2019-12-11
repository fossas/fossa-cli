module LabeledGraphing
  ( LabeledGraphing(..)
  , PkgLabel

  , empty
  , direct
  , edge
  , label
  , unlabel
  ) where

import Prologue hiding (empty, parent)

import           Data.Kind (Type)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Graphing as G

data LabeledGraphing ty = LabeledGraphing
  { labeledGraphing :: G.Graphing ty
  , labeledLabels   :: Map ty (Set (PkgLabel ty))
  } deriving Generic

type family PkgLabel ty :: Type

empty :: LabeledGraphing ty
empty = LabeledGraphing G.empty M.empty

direct :: Ord ty => ty -> LabeledGraphing ty -> LabeledGraphing ty
direct dep gr = gr { labeledGraphing = G.direct dep (labeledGraphing gr) }

edge :: Ord ty => ty -> ty -> LabeledGraphing ty -> LabeledGraphing ty
edge parent child gr = gr { labeledGraphing = G.edge parent child (labeledGraphing gr) }

label :: (Ord ty, Ord (PkgLabel ty)) => ty -> PkgLabel ty -> LabeledGraphing ty -> LabeledGraphing ty
label dep lbl gr = gr { labeledLabels = labels' }
  where
  labels' = M.insertWith (<>) dep (S.singleton lbl) (labeledLabels gr)

unlabel :: (Ord ty, Ord ty') => (ty -> Set (PkgLabel ty) -> ty') -> LabeledGraphing ty -> G.Graphing ty'
unlabel f lgr = G.gmap (\ty -> f ty (findLabels ty)) (labeledGraphing lgr)
  where
  findLabels ty = (fromMaybe S.empty (M.lookup ty (labeledLabels lgr)))
