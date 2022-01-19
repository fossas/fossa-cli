module App.Fossa.VSI.DynLinked.Util (
  isSetUID,
) where

import Control.Algebra (Has)
import Control.Effect.Lift (Lift, sendIO)
import Data.Bits ((.|.))
import Path (File, Path, toFilePath)
import System.Posix.Files (fileMode, getFileStatus, setUserIDMode)

-- | Test whether the file has a `setuid` bit.
isSetUID :: (Has (Lift IO) sig m) => Path t File -> m Bool
isSetUID file = do
  stat <- sendIO . getFileStatus $ toFilePath file
  pure $ fileMode stat .|. setUserIDMode == 0
