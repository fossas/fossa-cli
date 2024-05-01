module Strategy.Python.Poetry.Common (
  getPoetryBuildBackend,
  toMap,
  pyProjectDeps,
  logIgnoredDeps,
  toCanonicalName,

  -- * for testing
  supportedPyProjectDep,
  supportedPoetryLockDep,
) where

import Data.Foldable (asum, for_)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text, replace, toLower)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvOther, EnvProduction, EnvTesting),
  DepType (GitType, PipType, URLType),
  Dependency (..),
  VerConstraint (
    CEq
  ),
 )
import Effect.Logger (Has, Logger, Pretty (pretty), logDebug)
import Strategy.Python.Poetry.PoetryLock (PackageName (..), PoetryLock (..), PoetryLockPackage (..), PoetryLockPackageSource (..))
import Strategy.Python.Poetry.PyProject (
  PoetryDependency (..),
  PyProject (..),
  PyProjectBuildSystem (..),
  PyProjectPoetry (..),
  PyProjectPoetryDetailedVersionDependency (..),
  PyProjectPoetryGitDependency (..),
  PyProjectPoetryPathDependency (..),
  PyProjectPoetryUrlDependency (..),
  allPoetryNonProductionDeps,
  toDependencyVersion,
 )

-- | Gets build backend of pyproject.
getPoetryBuildBackend :: PyProject -> Maybe Text
getPoetryBuildBackend project = buildBackend <$> pyprojectBuildSystem project

-- | Supported pyproject dependencies.
supportedPyProjectDep :: PoetryDependency -> Bool
supportedPyProjectDep (PyProjectPoetryPathDependencySpec _) = False
supportedPyProjectDep _ = True

-- | Logs all ignored dependencies for debugger.
logIgnoredDeps :: Has Logger sig m => PyProject -> Maybe PoetryLock -> m ()
logIgnoredDeps pyproject poetryLock = for_ notSupportedDepsMsgs (logDebug . pretty)
  where
    notSupportedDepsMsgs :: [Text]
    notSupportedDepsMsgs = map (<> ": ignored in poetry project. Dependency's source is not supported!") notSupportedDeps

    notSupportedDeps :: [Text]
    notSupportedDeps = case poetryLock of
      Nothing -> notSupportedPyProjectDevDeps <> notSupportedPyProjectDeps
      Just pl -> map (unPackageName . poetryLockPackageName) $ filter (not . supportedPoetryLockDep) (poetryLockPackages pl)

    notSupportedPyProjectDevDeps :: [Text]
    notSupportedPyProjectDevDeps =
      Map.keys $
        Map.filter (not . supportedPyProjectDep) $
          allPoetryNonProductionDeps pyproject

    notSupportedPyProjectDeps :: [Text]
    notSupportedPyProjectDeps =
      Map.keys $
        Map.filter (not . supportedPyProjectDep) $
          maybe Map.empty dependencies (pyprojectPoetry pyproject)

-- | Not supported poetry lock package.
supportedPoetryLockDep :: PoetryLockPackage -> Bool
supportedPoetryLockDep pkg = Just "file" /= (poetryLockPackageSourceType <$> poetryLockPackageSource pkg)

-- | Gets pyproject dependencies.
pyProjectDeps :: PyProject -> [Dependency]
pyProjectDeps project = filter notNamedPython $ map snd allDeps
  where
    -- pyproject typically includes python as dependency that has to be ignored
    notNamedPython = (/= "python") . dependencyName

    supportedDevDeps :: Map Text PoetryDependency
    supportedDevDeps = Map.filter supportedPyProjectDep $ Map.unions [olderPoetryDevDeps, groupDeps]

    -- These are dependencies coming from dev-dependencies table
    -- which is pre 1.2.x style, understood by Poetry 1.0–1.2
    olderPoetryDevDeps :: Map Text PoetryDependency
    olderPoetryDevDeps = case pyprojectPoetry project of
      Just (PyProjectPoetry{devDependencies}) -> devDependencies
      _ -> mempty

    -- These are 'group' dependencies. All group dependencies are optional.
    -- Due to current toml parsing library limitation (specifically implicit table parsing support)
    -- We only support dev, and test group. We may miss other development dependencies in our findings
    -- if they are not named under 'dev' or 'test' group. This is not ideal, but is good partial solution
    -- as optional deps are not included in the final analysis by default.
    --
    -- Refs:
    -- \* https://github.com/kowainik/tomland/issues/336
    -- \* https://python-poetry.org/docs/managing-dependencies#dependency-groups
    groupDeps :: Map Text PoetryDependency
    groupDeps = case pyprojectPoetry project of
      Just (PyProjectPoetry{groupDevDependencies, groupTestDependencies}) -> Map.unions [groupDevDependencies, groupTestDependencies]
      _ -> mempty

    supportedProdDeps :: Map Text PoetryDependency
    supportedProdDeps = Map.filter supportedPyProjectDep $ maybe Map.empty dependencies (pyprojectPoetry project)

    toDependency :: [DepEnvironment] -> Map Text PoetryDependency -> Map Text Dependency
    toDependency depEnvs = Map.mapWithKey $ poetrytoDependency depEnvs

    allDeps :: [(Text, Dependency)]
    allDeps = Map.toList prodDeps ++ Map.toList devDeps
      where
        prodDeps = toDependency [EnvProduction] supportedProdDeps
        devDeps = toDependency [EnvDevelopment] supportedDevDeps

-- | Gets Dependency from `PoetryDependency` and it's `DepEnvironment`.
poetrytoDependency :: [DepEnvironment] -> Text -> PoetryDependency -> Dependency
poetrytoDependency depEnvs name deps =
  Dependency
    { dependencyType = depType
    , dependencyName = depName
    , dependencyVersion = depVersion
    , dependencyLocations = depLocations
    , dependencyEnvironments = Set.fromList depEnvironment
    , dependencyTags = depTags
    }
  where
    depType = case deps of
      PyProjectPoetryGitDependencySpec _ -> GitType
      PyProjectPoetryUrlDependencySpec _ -> URLType
      _ -> PipType

    depName = case deps of
      PoetryTextVersion _ -> name
      PyProjectPoetryDetailedVersionDependencySpec _ -> name
      PyProjectPoetryGitDependencySpec ds -> gitUrl ds
      PyProjectPoetryUrlDependencySpec ds -> sourceUrl ds
      PyProjectPoetryPathDependencySpec ds -> sourcePath ds

    depVersion = case deps of
      PoetryTextVersion ds -> toDependencyVersion ds
      PyProjectPoetryDetailedVersionDependencySpec ds -> toDependencyVersion (poetryDependencyVersion ds)
      PyProjectPoetryGitDependencySpec ds -> case asum [gitTag ds, gitRev ds, gitBranch ds] of
        Nothing -> Nothing
        Just version -> Just $ CEq version
      _ -> Nothing

    depEnvironment = depEnvs
    depLocations = []
    depTags = Map.empty

-- | Converts text to canonical python name for dependency.
-- Relevant Docs: https://www.python.org/dev/peps/pep-0426/#id28
-- Poetry Code: https://github.com/python-poetry/poetry/blob/master/poetry/utils/helpers.py#L35
--
-- Poetry performs this operation inconsistently at the time of writing for package name and it's dependencies
-- within the lock file.
--
--  ```toml
--  [package.dependencies]
--  MarkupSafe = ">=2.0"
--  ....
--
-- [[package]]
-- name = "markupsafe"
-- version = "2.0.1"
-- ...
-- ```
toCanonicalName :: Text -> Text
toCanonicalName t = toLower $ replace "_" "-" (replace "." "-" t)

-- | Maps poetry lock package to map of package name and associated dependency.
toMap :: [PackageName] -> [PoetryLockPackage] -> Map.Map PackageName Dependency
toMap prodPkgs pkgs = Map.fromList $ (\x -> (canonicalPkgName x, toDependency x)) <$> (filter supportedPoetryLockDep pkgs)
  where
    canonicalPkgName :: PoetryLockPackage -> PackageName
    canonicalPkgName pkg = PackageName $ toCanonicalName $ unPackageName $ poetryLockPackageName pkg

    canonicalPkgName' :: PackageName -> PackageName
    canonicalPkgName' = PackageName . toCanonicalName . unPackageName

    canonicalProdPkgNames :: [PackageName]
    canonicalProdPkgNames = map canonicalPkgName' prodPkgs

    isProductionDirectDep :: PoetryLockPackage -> Bool
    isProductionDirectDep pkg = canonicalPkgName pkg `elem` canonicalProdPkgNames

    toDependency :: PoetryLockPackage -> Dependency
    toDependency pkg =
      Dependency
        { dependencyType = toDepType (poetryLockPackageSource pkg)
        , dependencyName = toDepName pkg
        , dependencyVersion = toDepVersion pkg
        , dependencyLocations = toDepLocs pkg
        , dependencyEnvironments = toDepEnvironment pkg
        , dependencyTags = Map.empty
        }

    toDepName :: PoetryLockPackage -> Text
    toDepName lockPkg = case (poetryLockPackageSource lockPkg) of
      Nothing -> unPackageName $ poetryLockPackageName lockPkg
      Just lockPkgSrc -> case poetryLockPackageSourceType lockPkgSrc of
        "legacy" -> unPackageName $ poetryLockPackageName lockPkg
        _ -> poetryLockPackageSourceUrl lockPkgSrc

    toDepType :: Maybe PoetryLockPackageSource -> DepType
    toDepType Nothing = PipType
    toDepType (Just lockPkgSrc) = case poetryLockPackageSourceType lockPkgSrc of
      "git" -> GitType
      "url" -> URLType
      _ -> PipType

    toDepLocs :: PoetryLockPackage -> [Text]
    toDepLocs pkg = case poetryLockPackageSource pkg of
      Nothing -> []
      Just lockPkgSrc -> case poetryLockPackageSourceType lockPkgSrc of
        "legacy" -> [poetryLockPackageSourceUrl lockPkgSrc]
        _ -> []

    toDepVersion :: PoetryLockPackage -> Maybe VerConstraint
    toDepVersion pkg = Just $
      CEq $
        fromMaybe (poetryLockPackageVersion pkg) $ do
          lockPkgSrc <- poetryLockPackageSource pkg
          ref <- poetryLockPackageSourceReference lockPkgSrc
          if poetryLockPackageSourceType lockPkgSrc /= "legacy" then Just ref else Nothing

    toDepEnvironment :: PoetryLockPackage -> Set.Set DepEnvironment
    toDepEnvironment pkg = case poetryLockPackageCategory pkg of
      -- If category is provided, use category to infer if dependency's environment
      Just category -> case category of
        "dev" -> Set.singleton EnvDevelopment
        "main" -> Set.singleton EnvProduction
        "test" -> Set.singleton EnvTesting
        other -> Set.singleton $ EnvOther other
      -- If category is not provided, lockfile is likely greater than __.
      -- In this case, if the package name exists in the dependencies
      -- list, mark as production dependency, otherwise, mark it as development dependency
      -- -
      -- Refer to:
      -- \* https://github.com/python-poetry/poetry/pull/7637
      Nothing ->
        (if isProductionDirectDep pkg then Set.singleton EnvProduction else mempty)
