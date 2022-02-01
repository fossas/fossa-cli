module Control.Effect.Stack (
  -- * Stack effect and operations
  context,
  getStack,
  withEmptyStack,
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
--       getStack -- stack is ["bar","foo"] here
--
--     context "baz" $ do
--       getStack -- stack is ["baz","foo"] here
--
--     withEmptyStack . context "quux" $ do
--       getStack -- stack is ["quux"] here
-- @
data Stack m a where
  Context :: Text -> m a -> Stack m a
  GetStack :: Stack m [Text]
  WithEmptyStack :: m a -> Stack m a

-- | Push a scope onto the stack. When the inner action completes, the scope is popped
context :: Has Stack sig m => Text -> m a -> m a
context name m = send (Context name m)

-- | Get the current scope stack in FILO order
getStack :: Has Stack sig m => m [Text]
getStack = send GetStack

-- | Run the provided action with an empty callstack
withEmptyStack :: Has Stack sig m => m a -> m a
withEmptyStack = send . WithEmptyStack
