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
--     data FilesystemF a where
--       ReadFile :: FilePath -> FilesystemF String
--       WriteFile :: FilePath -> String -> FilesystemF ()
--
--     type Filesystem = Simple FilesystemF
-- @
--
-- @ReadFile@ and @WriteFile@ are data constructors for @FilesystemF@. They
-- describe the arguments and "result types" of each operation:
--
-- @
--     -- When reading a file, a string is returned
--     example1 :: FilesystemF String
--     example1 = ReadFile "/path/to/file"
--
--     -- When writing to a file, nothing is returned
--     example2 :: FilesystemF ()
--     example2 = WriteFile "/path/to/file" "content"
-- @
--
-- == Effect functions
--
-- 'sendSimple' is used to "invoke" effect operations:
--
-- @
--     example3 :: Has Filesystem sig m => m String
--     example3 = sendSimple (ReadFile "/path/to/file")
--
--     example4 :: Has Filesystem sig m => String -> m ()
--     example4 content = sendSimple (WriteFile "/path/to/file" content)
-- @
--
-- To make using your effect easier, create a helper function for each effect operation:
-- @
--     data FilesystemF a where
--       ReadFile :: FilePath -> FilesystemF String
--       WriteFile :: FilePath -> String -> FilesystemF ()
--
--     type Filesystem = Simple FilesystemF
--
--     readFile :: Has Filesystem sig m => FilePath -> m String
--     readFile file = sendSimple (ReadFile file)
--
--     writeFile :: Has Filesystem sig m => FilePath -> String -> m ()
--     writeFile file content = sendSimple (WriteFile file content)
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
-- To "run" an effect, we need to supply an interpreter (or "carrier", hence
-- the "C" in "FilesystemC" below). An interpreter tells us how to turn our
-- effect calls (which are pure datatypes) into effectful actions.
--
-- Most commonly, we can do this with 'interpret':
--
-- @
--     type FilesystemC = SimpleC FilesystemF
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
  Simple (..),
  SimpleC (..),
  SimpleStateC,
  sendSimple,
  sendSimple2,
  sendSimple3,
  interpret,
  interpretState,
  module X,
) where

import Control.Algebra as X
import Control.Applicative
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Monad.Trans
import Control.Monad.Except (MonadError (..))
import Conduit (MonadUnliftIO)

---------- The Simple effect

-- | First-order effects.. wrapped into @fused-effects@
--
-- First-order effect constructors are not allowed to contain effectful
-- sub-expressions as arguments.
--
-- Consider the @Error@ effect:
--
-- @
--     data Error e m a where
--       Throw :: e -> Error e m a
--       Catch :: m a -> (e -> m a) -> Error e m a
-- @
--
-- The @Catch@ constructor here takes effectful sub-expressions as arguments:
-- both of its arguments mention @m@.
--
-- In contrast, @Throw@ only takes the error type as an argument, with no
-- mention of @m@
--
-- As such, we can hypothetically break @Error@ out into two effects, one
-- first-order and one not.
--
-- @
--     ---- Throw
--     data ThrowF e a where
--       Throw :: e -> Throw e a
--
--     type Throw e = Simple (ThrowF e)
--
--     ---- Catch
--     data Catch e m a where
--       Catch :: m a -> (e -> m a) -> Error e m a
--
--     ---- Combining them
--     type Error e = Throw e :+: Catch e
-- @
data Simple e m a where
  Simple :: e a -> Simple e m a

-- | Invoke an effect constructor
sendSimple :: Has (Simple eff) sig m => eff a -> m a
sendSimple = send . Simple

-- | Invoke an effect constructor
sendSimple2 :: Has (Simple eff) sig m => (a -> b -> eff c) -> a -> b -> m c
sendSimple2 constructor arg = send . Simple . constructor arg

-- | Invoke an effect constructor
sendSimple3 :: Has (Simple eff) sig m => (a -> b -> c -> eff d) -> a -> b -> c -> m d
sendSimple3 constructor arg1 arg2 = send . Simple . constructor arg1 arg2

---------- Direct interpretation

-- | Given a function to transform any effect data constructor @eff x@ into an
-- action @m x@, interpret effect calls for @eff@
interpret :: (forall x. eff x -> m x) -> SimpleC eff m a -> m a
interpret f = runReader (HandlerFor f) . runSimpleC

---------- Stateful interpretation

-- | A convenient type alias for defining interpreters that need access to a
-- state value
type SimpleStateC s eff m = SimpleC eff (StateC s m)

-- | A wrapper for 'interpret' that gives access to a state value ('get', 'put',
-- 'modify', etc)
interpretState :: s -> (forall a. eff a -> StateC s m a) -> SimpleC eff (StateC s m) b -> m (s, b)
interpretState s f = runState s . interpret f

---------- Internals

-- | A carrier for arbitrary "first-order" effects
newtype SimpleC eff m a = SimpleC {runSimpleC :: ReaderC (HandlerFor eff m) m a}
  deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadUnliftIO)

-- | A wrapper for an effect handler function
data HandlerFor eff m where
  HandlerFor :: (forall a. eff a -> m a) -> HandlerFor eff m

instance MonadTrans (SimpleC eff) where
  lift = SimpleC . lift

instance (MonadError e m, Algebra sig m) => MonadError e (SimpleC eff m) where
  throwError = lift . throwError
  catchError action handler = do
    (HandlerFor f) <- SimpleC ask
    lift $ catchError (interpret f action) (interpret f . handler)

instance Algebra sig m => Algebra (Simple eff :+: sig) (SimpleC eff m) where
  alg hdl sig ctx = SimpleC $ do
    case sig of
      L (Simple ours) -> ReaderC $ \(HandlerFor g) -> do
        res <- g ours
        pure (res <$ ctx)
      R other -> alg (runSimpleC . hdl) (R other) ctx
