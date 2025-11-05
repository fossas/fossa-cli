{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.DebugDir (globalDebugDirRef) where

import Data.IORef (IORef, newIORef)
import System.IO.Unsafe (unsafePerformIO)

-- | Global IORef to store the debug directory path
-- This is used to pass the debug directory out of the effect stack
-- so the telemetry sink can write directly to it
{-# NOINLINE globalDebugDirRef #-}
globalDebugDirRef :: IORef (Maybe FilePath)
globalDebugDirRef = unsafePerformIO $ newIORef Nothing

