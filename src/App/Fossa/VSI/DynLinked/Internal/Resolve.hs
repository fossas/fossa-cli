{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSI.DynLinked.Internal.Resolve (
  toSourceUnit,
  toDependency,
) where

import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.BinaryDeps (analyzeSingleBinary)
import App.Fossa.VSI.DynLinked.Types (DynamicDependency (..), LinuxDistro (..), LinuxPackageManager (..), LinuxPackageMetadata (..), ResolvedLinuxPackage (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Data.Either (partitionEithers)
import Data.Set (Set, toList)
import Data.Text (Text, intercalate)
import DepTypes (DepType (LinuxAPK, LinuxDEB, LinuxRPM), Dependency (..), VerConstraint (CEq))
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, Dir, File, Path)
import Srclib.Converter qualified as Srclib
import Srclib.Types (AdditionalDepData (..), SourceUnit (..), SourceUserDefDep)
import Types (DiscoveredProjectType (VsiProjectType), GraphBreadth (Complete))

-- | Resolves a set of dynamic dependencies into a @SourceUnit@.
-- Any @DynamicDependency@ that isn't resolved to a Linux package dependency is converted to an unknown binary dependency.
toSourceUnit ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  LinuxDistro ->
  Set DynamicDependency ->
  m SourceUnit
toSourceUnit root distro dependencies = do
  let (resolved, unresolved) = sortResolvedUnresolved dependencies
  binaries <- traverse (analyzeSingleBinary root) unresolved

  let project = toProject root . Graphing.directs $ map (toDependency distro) resolved
  let unit = Srclib.toSourceUnit False project
  pure $ unit{additionalData = fmap toDepData (Just binaries)}
  where
    toDepData :: [SourceUserDefDep] -> AdditionalDepData
    toDepData d = AdditionalDepData (Just d) Nothing
    toProject :: Path Abs Dir -> Graphing Dependency -> ProjectResult
    toProject dir graph = ProjectResult VsiProjectType dir graph Complete []

toDependency :: LinuxDistro -> ResolvedLinuxPackage -> Dependency
toDependency distro ResolvedLinuxPackage{..} = case resolvedLinuxPackageManager of
  LinuxPackageManagerDEB -> renderDEB resolvedLinuxPackageMetadata distro
  LinuxPackageManagerRPM -> renderRPM resolvedLinuxPackageMetadata distro
  LinuxPackageManagerAPK -> renderAPK resolvedLinuxPackageMetadata distro
  where
    render :: DepType -> [Text] -> [Text] -> Dependency
    render fetcher projectParts revisionParts =
      Dependency
        { dependencyType = fetcher
        , dependencyName = fromParts projectParts
        , dependencyVersion = Just . CEq $ fromParts revisionParts
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = mempty
        }

    fromParts :: [Text] -> Text
    fromParts = intercalate "#"

    renderDEB :: LinuxPackageMetadata -> LinuxDistro -> Dependency
    renderDEB LinuxPackageMetadata{..} LinuxDistro{..} =
      render
        LinuxDEB
        [linuxPackageID, linuxDistroName, linuxDistroRelease]
        [linuxPackageArch, linuxPackageRevision]

    renderRPM :: LinuxPackageMetadata -> LinuxDistro -> Dependency
    renderRPM LinuxPackageMetadata{..} LinuxDistro{..} =
      render
        LinuxRPM
        [linuxPackageID, linuxDistroName, linuxDistroRelease]
        [linuxPackageArch, epoch <> linuxPackageRevision]

    renderAPK :: LinuxPackageMetadata -> LinuxDistro -> Dependency
    renderAPK LinuxPackageMetadata{..} LinuxDistro{..} =
      render
        LinuxAPK
        [linuxPackageID, linuxDistroName, linuxDistroRelease]
        [linuxPackageArch, linuxPackageRevision]

    epoch :: Text
    epoch = maybe "" (<> ":") $ linuxPackageDistroEpoch resolvedLinuxPackageMetadata

-- | Sort @Set DynamicDependency@ into two lists: "resolved", and "unresolved", in a single pass.
--
-- Whether a dependency is "resolved" is predicated on whether its @dynamicDependencyResolved@ is not @Nothing@.
-- Dependencies thus sorted are unwrapped into more specific types based on the needs of downstream functions
-- converting this set of dependencies into a @SourceUnit@.
sortResolvedUnresolved :: Set DynamicDependency -> ([ResolvedLinuxPackage], [Path Abs File])
sortResolvedUnresolved = partitionEithers . map forkEither . toList
  where
    forkEither :: DynamicDependency -> Either ResolvedLinuxPackage (Path Abs File)
    forkEither dep = case dynamicDependencyResolved dep of
      Nothing -> Right $ dynamicDependencyDiskPath dep
      Just linuxPackage -> Left linuxPackage
