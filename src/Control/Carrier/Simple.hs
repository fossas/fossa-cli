{-# LANGUAGE UndecidableInstances #-}

-- | A friendlier interface for implementing effects
--
-- Creating effects and effect handlers ("carriers") is intimidating and
-- needlessly complex for most effect types. This module (mostly) solves that
-- problem.
--
-- = Defining effects
--
-- == Datatype
--
-- Effects are defined with a datatype describing its operations. For example, a
-- datatype for filesystem read/write operations could look like this:
--
-- @
--     data Filesystem m a where
--       ReadFile :: FilePath -> Filesystem m String
--       WriteFile :: FilePath -> String -> Filesystem m ()
-- @
--
-- The @m@ in this type can be ignored; it's used only in complex effects.
--
-- @ReadFile@ and @WriteFile@ are data constructors for @Filesystem@. They
-- describe the arguments and "result types" of each operation:
--
-- @
--     -- When reading a file, a string is returned
--     example1 :: Filesystem m String
--     example1 = ReadFile "/path/to/file"
--
--     -- When writing to a file, nothing is returned
--     example2 :: Filesystem m ()
--     example2 = WriteFile "/path/to/file" "content"
-- @
--
-- == Effect functions
--
-- 'Control.Algebra.send' is used to "invoke" effect operations:
--
-- @
--     example3 :: Has Filesystem sig m => m String
--     example3 = send (ReadFile "/path/to/file")
--
--     example4 :: Has Filesystem sig m => String -> m ()
--     example4 content = send (WriteFile "/path/to/file" content)
-- @
--
-- To make using your effect easier, create a helper function for each effect operation:
-- @
--     data Filesystem m a where
--       ReadFile :: FilePath -> Filesystem m String
--       WriteFile :: FilePath -> String -> Filesystem m ()
--
--     readFile :: Has Filesystem sig m => FilePath -> m String
--     readFile file = send (ReadFile file)
--
--     writeFile :: Has Filesystem sig m => FilePath -> String -> m ()
--     writeFile file content = send (WriteFile file content)
-- @
--
-- Users can compose these functions to easily use your effect:
-- @
--     addNewlineToEndOfFile :: Has Filesystem sig m => FilePath -> m ()
--     addNewlineToEndOfFile file = do
--       content <- readFile file
--       writeFile file (content ++ "\n")
-- @
--
-- == Interpreting effects
--
-- To "run" an effect, we need to supply an interpreter. An interpreter tells us
-- how to turn our effect calls (which are pure datatypes) into effectful
-- actions.
--
-- Most commonly, we can do this with 'interpret':
--
-- @
--     type FilesystemC = SimpleC Filesystem
--
--     runFilesystemIO :: Has (Lift IO) sig m => FilesystemC m a -> m a
--     runFilesystemIO = interpret $ \case
--       ReadFile file -> sendIO (Prelude.readFile file)
--       WriteFile file content -> sendIO (Prelude.writeFile file content)
--
--     runFilesystemDumb :: Algebra sig m => FilesystemC m a -> m a
--     runFilesystemDumb = interpret $ \case
--       ReadFile _ -> pure "content!"
--       WriteFile _ _ -> pure ()
-- @
--
-- Here we define two interpreters: one that uses @IO@ actions, and another that
-- doesn't do much of anything.
--
-- The function we pass to @interpret@ tells us how to handle each effect call.
--
-- @
--     -- does the thing!
--     example5 :: IO ()
--     example5 = runM $ runFilesystemIO (addNewlineToEndOfFile "/path/to/file")
--
--     -- does nothing
--     example6 :: ()
--     example6 = run $ runFilesystemDumb (addNewlineToEndOfFile "/path/to/file")
--
--     -- "content!"
--     example7 :: String
--     example7 = run $ runFilesystemDumb (readFile "/path/to/file")
-- @
module Control.Carrier.Simple (
  SimpleC (..),
  SimpleStateC,
  interpret,
  interpretState,
  module X,
) where

import Control.Algebra as X
import Control.Applicative
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Monad.Trans
import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)

---------- Direct interpretation

-- | Given a function to turn effect calls @eff a@ into actions @m a@, interpret effect calls for @eff@
interpret :: (forall x. eff Unused x -> m x) -> SimpleC eff m a -> m a
interpret f = runReader (HandlerFor f) . runSimpleC

---------- Stateful interpretation

-- | A convenient type alias for defining interpreters that need access to a state value
type SimpleStateC s eff m = SimpleC eff (StateC s m)

-- | A wrapper for 'interpret' that gives access to a state value ('get', 'put', 'modify', etc)
interpretState :: s -> (forall a. eff Unused a -> StateC s m a) -> SimpleC eff (StateC s m) b -> m (s, b)
interpretState s f = runState s . interpret f

---------- Internals

-- | A carrier for arbitrary "first-order" effects (i.e. those that don't use
-- the @m@ in @data MyEffect m a@)
newtype SimpleC (eff :: (Type -> Type) -> Type -> Type) m a = SimpleC {runSimpleC :: ReaderC (HandlerFor eff m) m a}
  deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadIO)

-- | A wrapper for an effect handler function
data HandlerFor (eff :: (Type -> Type) -> Type -> Type) m where
  HandlerFor :: (eff Unused a -> m a) -> HandlerFor eff m

-- | Like 'Data.Void.Void', but with kind @Type -> Type@
data Unused (a :: Type)

instance MonadTrans (SimpleC eff) where
  lift = SimpleC . lift

instance Algebra sig m => Algebra (eff :+: sig) (SimpleC eff m) where
  alg hdl sig ctx = SimpleC $ do
    case sig of
      L ours -> do
        HandlerFor g <- ask @(HandlerFor eff m)
        res <- lift $ unsafeCoerce g ours
        pure (res <$ ctx)
      R other -> alg (runSimpleC . hdl) (R other) ctx
