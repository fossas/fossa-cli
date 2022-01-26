module App.Fossa.VSI.DynLinked.Internal.Lookup (
  dynamicDependencies,
) where

import App.Fossa.VSI.DynLinked.Types (DynamicDependency, ResolvedLinuxPackage)
import Data.Set (Set, empty, toList)
import Path (Abs, File, Path)
import System.Info qualified as SysInfo

dynamicDependencies :: Monad m => Set (Path Abs File) -> m (Set DynamicDependency)
dynamicDependencies files = undefined
