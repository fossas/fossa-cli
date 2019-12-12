{-# language TemplateHaskell #-}

-- | Grapher is a thin State wrapper effect around 'G.Graphing'
--
-- It defines @direct@ and @edge@ combinators analagous to 'G.direct' and
-- 'G.edge' from 'G.Graphing'
module Effect.Grapher
  ( Grapher(..)
  , direct
  , edge

  , evalGrapher
  , runGrapher

  ) where

import Prologue hiding (parent)

import Polysemy
import Polysemy.State

import qualified Graphing as G

data Grapher ty m a where
  Direct :: ty -> Grapher ty m ()
  Edge :: ty -> ty -> Grapher ty m ()

makeSem ''Grapher

evalGrapher :: Ord ty => Sem (Grapher ty ': r) a -> Sem r (G.Graphing ty)
evalGrapher = fmap fst . runGrapher

runGrapher :: Ord ty => Sem (Grapher ty ': r) a -> Sem r (G.Graphing ty, a)
runGrapher = runState G.empty . reinterpret
  (\case
      Direct ty -> modify (G.direct ty)
      Edge parent child -> modify (G.edge parent child))
