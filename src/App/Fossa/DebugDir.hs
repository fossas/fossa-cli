module App.Fossa.DebugDir (
  DebugDirRef,
  globalDebugDirRef,
  newDebugDirRef,
  readDebugDir,
  writeDebugDir,
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

-- | Type alias for the debug directory reference
type DebugDirRef = IORef (Maybe FilePath)

-- | Global debug directory reference.
-- This is initialized in Subcommand.hs at the start of each command.
-- While this uses unsafePerformIO, it's safe because:
-- 1. It's only written once at the start of command execution
-- 2. It's read-only after initialization
-- 3. The telemetry sink gets the IORef passed explicitly (no unsafe access there)
{-# NOINLINE globalDebugDirRef #-}
globalDebugDirRef :: DebugDirRef
globalDebugDirRef = unsafePerformIO $ newIORef Nothing

-- | Create a new debug directory reference
newDebugDirRef :: IO DebugDirRef
newDebugDirRef = newIORef Nothing

-- | Read the debug directory from the reference
readDebugDir :: DebugDirRef -> IO (Maybe FilePath)
readDebugDir = readIORef

-- | Write the debug directory to the reference
writeDebugDir :: DebugDirRef -> Maybe FilePath -> IO ()
writeDebugDir = writeIORef
