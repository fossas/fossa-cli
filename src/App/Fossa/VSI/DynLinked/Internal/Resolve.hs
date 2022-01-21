{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSI.DynLinked.Internal.Resolve (
  toSourceUnit,
  toDependency,
) where

import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.BinaryDeps (analyzeSingleBinary)
import App.Fossa.VSI.DynLinked.Types (DynamicDependency (..), LinuxPackageManager (..), LinuxPackageMetadata (..), ResolvedLinuxPackage (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Data.Set (Set, toList)
import Data.Text (intercalate)
import DepTypes (DepType (LinuxAPK, LinuxDEB, LinuxRPM), Dependency (..), VerConstraint (CEq))
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import Graphing qualified
import Path (Abs, Dir, File, Path)
import Srclib.Converter qualified as Srclib
import Srclib.Types (AdditionalDepData (..), SourceUnit (..))
import Types (DiscoveredProjectType (VsiProjectType), GraphBreadth (Complete))

-- | Resolves a set of dynamic dependencies into a @SourceUnit@.
-- Any @DynamicDependency@ that isn't resolved to a Linux package dependency is converted to an unknown binary dependency.
toSourceUnit :: (Has (Lift IO) sig m, Has Logger sig m, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Set DynamicDependency -> m SourceUnit
toSourceUnit root dependencies = do
  let (resolved, unresolved) = sortResolvedUnresolved dependencies
  binaries <- traverse (analyzeSingleBinary root) unresolved

  let project = toProject root $ Graphing.fromList (map toDependency resolved)
  let unit = Srclib.toSourceUnit False project
  pure $ unit{additionalData = fmap toDepData (Just binaries)}
  where
    toDepData d = AdditionalDepData (Just d) Nothing
    toProject dir graph = ProjectResult VsiProjectType dir graph Complete []

toDependency :: ResolvedLinuxPackage -> Dependency
toDependency ResolvedLinuxPackage{..} = case resolvedLinuxPackageManager of
  LinuxPackageManagerDEB -> renderDEB resolvedLinuxPackageMetadata
  LinuxPackageManagerRPM -> renderRPM resolvedLinuxPackageMetadata
  LinuxPackageManagerAPK -> renderAPK resolvedLinuxPackageMetadata
  where
    render fetcher projectParts revisionParts =
      Dependency
        { dependencyType = fetcher
        , dependencyName = fromParts projectParts
        , dependencyVersion = Just . CEq $ fromParts revisionParts
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = mempty
        }
    fromParts = intercalate "#"
    renderDEB LinuxPackageMetadata{..} =
      render
        LinuxDEB
        [linuxPackageID, linuxPackageDistro, linuxPackageDistroRelease]
        [linuxPackageArch, linuxPackageRevision]
    renderRPM LinuxPackageMetadata{..} =
      render
        LinuxRPM
        [linuxPackageID, linuxPackageDistro, linuxPackageDistroRelease]
        [linuxPackageArch, epoch <> linuxPackageRevision]
    renderAPK LinuxPackageMetadata{..} =
      render
        LinuxAPK
        [linuxPackageID, linuxPackageDistro, linuxPackageDistroRelease]
        [linuxPackageArch, linuxPackageRevision]
    epoch = maybe "" (<> ":") $ linuxPackageDistroEpoch resolvedLinuxPackageMetadata

-- | Sort @Set DynamicDependency@ into two lists: "resolved", and "unresolved", in a single pass.
--
-- Whether a dependency is "resolved" is predicated on whether its @dynamicDependencyResolved@ is not @Nothing@.
-- Dependencies thus sorted are unwrapped into more specific types based on the needs of downstream functions
-- converting this set of dependencies into a @SourceUnit@.
--
-- This function reverses the order of dependencies for performance reasons,
-- but dependency order isn't significant so we don't reverse it back.
sortResolvedUnresolved :: Set DynamicDependency -> ([ResolvedLinuxPackage], [Path Abs File])
sortResolvedUnresolved = buckets [] [] . toList
  where
    buckets resolved unresolved [] = (resolved, unresolved)
    buckets resolved unresolved (dep : remaining) = case dynamicDependencyResolved dep of
      Nothing -> buckets resolved (dynamicDependencyDiskPath dep : unresolved) remaining
      Just linuxPackage -> buckets (linuxPackage : resolved) unresolved remaining
