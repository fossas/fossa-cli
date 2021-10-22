module Control.Concurrent.Extra (
  initCapabilities,
) where

import Control.Exception (SomeException)
import Control.Exception.Extra (safeCatch)
import GHC.Conc (getNumProcessors, setNumCapabilities)
import System.CGroup

-- | Sets the number of available capabilities via 'GHC.Conc.setNumCapabilities'
--
-- On most platforms, this sets the number of capabilities to the number of
-- physical processors ('GHC.Conc.getNumProcessors').
--
-- When running within a cgroup on linux (most often within a container), this
-- takes into account the available cpu quota
initCapabilities :: IO ()
initCapabilities =
  initCapabilitiesFromCGroup
    `safeCatch` (\(_ :: SomeException) -> defaultInitCapabilities)

initCapabilitiesFromCGroup :: IO ()
initCapabilitiesFromCGroup = do
  cpuController <- resolveCPUController
  cgroupCpuQuota <- getCPUQuota cpuController
  case cgroupCpuQuota of
    NoQuota -> defaultInitCapabilities
    CPUQuota quota period -> do
      procs <- getNumProcessors
      let capabilities = clamp 1 procs (quota `div` period)
      setNumCapabilities capabilities

-- | Set number of capabilities to the number of available processors
defaultInitCapabilities :: IO ()
defaultInitCapabilities = setNumCapabilities =<< getNumProcessors

-- | Clamp a value within a range
clamp :: Int -> Int -> Int -> Int
clamp lower upper = max lower . min upper
