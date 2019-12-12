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

-- | A @LabeledGraphing ty@ is an extension of 'G.Graphing', where each node
-- also has a set of associated labels. LabeledGraphing can be transformed
-- into a Graphing using 'unlabel'
data LabeledGraphing ty = LabeledGraphing
  { labeledGraphing :: G.Graphing ty
  , labeledLabels   :: Map ty (Set (PkgLabel ty))
  } deriving Generic

-- | A type family of "package labels" to help drive type inference.
--
-- For example, given a node type of:
--
-- > data PipPkg = PipPkg { pipPkgName :: Text, pipPkgVersion :: Text }
-- >   deriving (Eq, Ord, Show)
--
-- We also want to keep track of labels we encounter:
--
-- > data PipLabel =
-- >     PipLocation Text
-- >   | PipEnvironment Text
-- >   deriving (Eq, Ord, Show)
--
-- To use @PipLabel@ as our label type in @LabeledGraphing PipPkg@, we declare:
--
-- > type instance PkgLabel PipPkg = PipLabel
--
-- Now, the type of the 'label' function is inferred to:
--
-- > label :: PipPkg -> PipLabel -> LabeledGraphing PipPkg -> LabeledGraphing PipPkg
type family PkgLabel ty :: Type

-- | The empty LabeledGraphing
empty :: LabeledGraphing ty
empty = LabeledGraphing G.empty M.empty

-- | Add a direct dependency to this LabeledGraphing. See 'G.direct'
direct :: Ord ty => ty -> LabeledGraphing ty -> LabeledGraphing ty
direct dep gr = gr { labeledGraphing = G.direct dep (labeledGraphing gr) }

-- | Add an edge between two nodes in this LabeledGraphing. See 'G.edge'
edge :: Ord ty => ty -> ty -> LabeledGraphing ty -> LabeledGraphing ty
edge parent child gr = gr { labeledGraphing = G.edge parent child (labeledGraphing gr) }

-- | Add a label to a node in this LabeledGraphing. An 'Ord' constraint exists
-- on labels so they can be deduplicated.
label :: (Ord ty, Ord (PkgLabel ty)) => ty -> PkgLabel ty -> LabeledGraphing ty -> LabeledGraphing ty
label dep lbl gr = gr { labeledLabels = labels' }
  where
  labels' = M.insertWith (<>) dep (S.singleton lbl) (labeledLabels gr)

-- | Transform a @LabeledGraphing ty@ to a @Graphing ty'@, given a function that
-- transforms the old node type @ty@ and a set of labels on that node
-- @Set (PkgLabel ty)@ to a new node type @ty'@
unlabel :: (Ord ty, Ord ty') => (ty -> Set (PkgLabel ty) -> ty') -> LabeledGraphing ty -> G.Graphing ty'
unlabel f lgr = G.gmap (\ty -> f ty (findLabels ty)) (labeledGraphing lgr)
  where
  findLabels ty = fromMaybe S.empty (M.lookup ty (labeledLabels lgr))
