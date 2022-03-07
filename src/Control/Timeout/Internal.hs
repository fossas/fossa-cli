-- Expose the Cancel internals for low-level usage and testing.

module Control.Timeout.Internal (
  Cancel (..),
) where

import Control.Concurrent (MVar)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import GHC.Generics (Generic)

-- Opaque wrapper around MVar (sort of like an atomic variable)
-- Only created by using `timeout'`
newtype Cancel = Cancel (MVar ()) deriving (Eq)
