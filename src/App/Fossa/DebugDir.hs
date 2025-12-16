module App.Fossa.DebugDir (
  DebugDirRef,
  newDebugDirRef,
  readDebugDir,
  writeDebugDir,
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | Type alias for the debug directory reference
type DebugDirRef = IORef (Maybe FilePath)

-- | Create a new debug directory reference
newDebugDirRef :: IO DebugDirRef
newDebugDirRef = newIORef Nothing

-- | Read the debug directory from the reference
readDebugDir :: DebugDirRef -> IO (Maybe FilePath)
readDebugDir = readIORef

-- | Write the debug directory to the reference
writeDebugDir :: DebugDirRef -> Maybe FilePath -> IO ()
writeDebugDir = writeIORef
