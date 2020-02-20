
module Prologue
  ( module X
  , fromEither
  ) where

-- 'log' conflicts with our logging effect
-- the others are partial functions
import Prelude as X hiding ((!!), foldl1, foldr1, head, init, last, log, maximum, minimum, tail)

import Debug.Trace as X (traceM, traceShow, traceShowId)

import Control.Applicative as X hiding (many, some)
import Control.Monad as X
import Control.Monad.IO.Class as X
import Control.Effect.Error
import Data.Aeson as X hiding (Error)
import Data.Bifunctor as X
import Data.Bool as X
import Data.ByteString as X (ByteString)
import Data.Coerce as X (coerce)
import Data.Foldable as X
import Data.Function as X ((&))
import Data.List as X (isPrefixOf, isSuffixOf)
import Data.Map.Strict as X (Map)
import Data.Maybe as X (fromMaybe)
import Data.Set as X (Set)
import Data.Text as X (Text)
import Data.Traversable as X
import Data.Void as X

import Path as X

import Data.Typeable as X (Typeable)
import GHC.Generics as X (Generic, Generic1)

fromEither :: Has (Error e) sig m => Either e a -> m a
fromEither = either throwError pure
