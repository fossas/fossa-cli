{-# LANGUAGE CPP #-}

module App.Fossa.VSI.DynLinked.Util (
  isSetUID,
) where

import Control.Algebra (Has)
import Control.Effect.Lift (Lift)

#ifdef mingw32_HOST_OS

-- | Test whether the file has a `setuid` bit.
-- Windows doesn't have the concept of a "set uid bit", so always eval to false.
isSetUID :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path t File -> m Bool
isSetUID _ = pure False

#else

import Control.Effect.Lift qualified as EL -- qualified to get the linter to stop yelling about combining imports
import Data.Bits ((.|.))
import Path (File, Path, toFilePath)
import System.Posix.Files (fileMode, getFileStatus, setUserIDMode)
import Control.Effect.Diagnostics (fatalOnSomeException, Diagnostics)

-- | Test whether the file has a `setuid` bit.
isSetUID :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path t File -> m Bool
isSetUID file = do
  stat <- fatalOnSomeException "get file status" . EL.sendIO . getFileStatus $ toFilePath file
  pure $ fileMode stat .|. setUserIDMode == 0

#endif
