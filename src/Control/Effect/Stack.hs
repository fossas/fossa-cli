module Control.Effect.Stack (
  -- * Stack effect and operations
  context,
  getStack,
  Stack (..),

  -- * Re-exports
  module X,
) where

import Control.Algebra as X
import Data.Text (Text)

-- | The stack effect allows for "scoped" context for effectful code, similar to
-- a call stack
--
-- Semantically, context elements are additive and.. scoped:
--
-- @
--   example = context "foo" $ do
--     getStack -- stack is ["foo"] here
--
--     context "bar" $ do
--       getStack -- stack is ["foo","bar"] here
--
--     context "baz" $ do
--       getStack -- stack is ["foo","baz"] here
-- @
data Stack m a where
  Context :: Text -> m a -> Stack m a
  GetStack :: Stack m [Text]

-- | Push a scope onto the stack. When the inner action completes, the scope is popped
context :: Has Stack sig m => Text -> m a -> m a
context name m = send (Context name m)

-- | Get the current scope stack
getStack :: Has Stack sig m => m [Text]
getStack = send GetStack
