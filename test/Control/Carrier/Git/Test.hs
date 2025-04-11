{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Git.Test
  ( GitTest,
    runGitTest,
    GitCommand,
  )
where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Git
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

type GitCommand = (String, Either String Text)

newtype GitTest m a = GitTest
  { runGitTest' :: ReaderC (Map String (Either String Text)) m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m) => Algebra (Git :+: sig) (GitTest m) where
  alg hdl sig ctx = GitTest $ case sig of
    L (Git args k) -> do
      responses <- ask
      let cmdStr = unwords args
      let response = Map.findWithDefault (Left $ "No mock response for: " ++ cmdStr) cmdStr responses
      pure (k response ctx)
    R other -> ReaderC $ \env -> alg (runGitTest' . hdl) (R other) ctx

runGitTest :: [GitCommand] -> GitTest m a -> m a
runGitTest responses = (`runReader` Map.fromList responses) . runGitTest' 