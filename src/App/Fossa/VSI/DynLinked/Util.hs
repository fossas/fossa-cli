{-# LANGUAGE CPP #-}

module App.Fossa.VSI.DynLinked.Util (
  hasSetUID,
) where

import Path (File, Path)

#ifdef mingw32_HOST_OS

-- | Test whether the file has a `setuid` bit.
-- Windows doesn't have the concept of a "set uid bit", so always eval to false.
hasSetUID :: Monad m => Path t File -> m Bool
hasSetUID _ = pure False

#else

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fatalOnSomeException)
import Control.Effect.Lift (Lift, sendIO)
import Data.Bits ((.&.))
import Path qualified as P
import System.Posix.Files (fileMode, getFileStatus, setUserIDMode)

-- | Test whether the file has a `setuid` bit.
hasSetUID :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path t File -> m Bool
hasSetUID file = do
  stat <- fatalOnSomeException "get file status" . sendIO . getFileStatus $ P.toFilePath file
  pure $ fileMode stat .&. setUserIDMode /= 0

#endif
