module Strategy.Python.PDM.PdmLock (
  PdmLock (..),
  PdmLockPackage (..),
  buildGraph,
  toDependency,
) where

import Data.Foldable (for_)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Set (Set, singleton)
import Data.Text (Text)
import DepTypes (DepEnvironment (EnvDevelopment, EnvProduction), DepType (GitType, PipType, URLType, UnresolvedPathType), Dependency (..), VerConstraint (..), hydrateDepEnvs)
import Effect.Grapher (deep, direct, edge, evalGrapher, run)
import Graphing (Graphing, gmap)
import Strategy.Python.Util (Req (..))
import Toml.Schema qualified

-- | Represents pdm lock file.
--
-- It roughly follows,
-- >
-- > [[package]]
-- > ...
-- > [[package]]
-- > ...
-- > [metadata]
-- > ...
-- >
-- > [metadata.files]
-- > ...
newtype PdmLock = PdmLock {pdmLockPackages :: [PdmLockPackage]}
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue PdmLock where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PdmLock
        <$> Toml.Schema.pickKey
          [ Toml.Schema.Key "package" Toml.Schema.fromValue
          , Toml.Schema.Else (pure [])
          ]

-- | Represents pdm lock package.
--
-- In pdm.lock file it looks like following:
-- -
-- > [[package]]
-- > name = "click"
-- > version = "8.1.3"
-- > requires_python = ">=3.7"
-- > summary = "Composable command line interface toolkit"
-- > dependencies = [
-- >     "colorama; platform_system == \"Windows\"",
-- > ]
data PdmLockPackage = PdmLockPackage
  { name :: Text
  , version :: Text
  , gitUrl :: Maybe Text
  , gitRevision :: Maybe Text
  , fsPath :: Maybe Text
  , url :: Maybe Text
  , pdmDependencies :: Maybe [Req]
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue PdmLockPackage where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PdmLockPackage
        <$> Toml.Schema.reqKey "name"
        <*> Toml.Schema.reqKey "version"
        <*> Toml.Schema.optKey "git"
        <*> Toml.Schema.optKey "revision"
        <*> Toml.Schema.optKey "path"
        <*> Toml.Schema.optKey "url"
        <*> Toml.Schema.optKey "dependencies"

toDependency :: [Req] -> [Req] -> PdmLockPackage -> Dependency
toDependency prodReqs devReqs pkg =
  Dependency
    { dependencyType = depType
    , dependencyName = depName
    , dependencyVersion = depVersion
    , dependencyLocations = depLocation
    , dependencyEnvironments = depEnv
    , dependencyTags = mempty
    }
  where
    depType :: DepType
    depType = case (gitUrl pkg, gitRevision pkg, fsPath pkg, url pkg) of
      (Just _, Just _, _, _) -> GitType
      (_, _, Just _, _) -> UnresolvedPathType
      (_, _, _, Just _) -> URLType
      _ -> PipType

    depLocation :: [Text]
    depLocation = case (fsPath pkg) of
      Just p -> [p]
      _ -> mempty

    depName :: Text
    depName = case (gitUrl pkg, fsPath pkg, url pkg) of
      (Just url, _, _) -> url
      (_, _, Just u) -> u
      (_, Just p, _) -> p
      _ -> name pkg

    depVersion :: Maybe VerConstraint
    depVersion = case (gitUrl pkg, gitRevision pkg) of
      (Just _, Just rev) -> Just . CEq $ rev
      _ -> Just . CEq $ version pkg

    depEnv :: Set DepEnvironment
    depEnv = case (gitUrl pkg, matchedProdReq, matchedDevReq) of
      (_, Nothing, Nothing) -> mempty
      (_, Nothing, Just _) -> singleton EnvDevelopment
      (Just _, Just r', _) ->
        if isUrlReq r'
          then singleton EnvProduction
          else mempty
      (_, Just _, _) -> singleton EnvProduction

    matchedProdReq :: Maybe Req
    matchedProdReq = find (\pr -> reqName pr == name pkg) prodReqs

    matchedDevReq :: Maybe Req
    matchedDevReq = find (\dr -> reqName dr == name pkg) devReqs

    isUrlReq :: Req -> Bool
    isUrlReq (UrlReq{}) = True
    isUrlReq _ = False

reqName :: Req -> Text
reqName (NameReq rname _ _ _) = rname
reqName (UrlReq rname _ _ _) = rname

buildGraph :: [Req] -> [Req] -> PdmLock -> Graphing Dependency
buildGraph prodReqs devReqs pdmLock = hydrateDepEnvs $
  gmap (toDependency prodReqs devReqs) $
    run . evalGrapher $ do
      for_ allDeps $ \resolvedDep -> do
        if isDirect resolvedDep
          then direct resolvedDep
          else deep resolvedDep

        let transitives = getTransitives resolvedDep
        for_ transitives $ \childDep -> do
          deep childDep
          edge resolvedDep childDep
  where
    allDeps :: [PdmLockPackage]
    allDeps = pdmLockPackages pdmLock

    isDirect :: PdmLockPackage -> Bool
    isDirect (PdmLockPackage name _ _ _ _ _ _) = any (\rr -> reqName rr == name) (prodReqs <> devReqs)

    -- Since pdm does not allow dependency of two
    -- different revision at the time of writing, so
    -- we can search by name, and disregard version constraint.
    getTransitives :: PdmLockPackage -> [PdmLockPackage]
    getTransitives pkg = case pdmDependencies pkg of
      Nothing -> []
      Just transitives -> mapMaybe findMatchingPackage transitives

    findMatchingPackage :: Req -> Maybe PdmLockPackage
    findMatchingPackage (UrlReq{}) = Nothing
    findMatchingPackage (NameReq rname _ _ _) = find (\dep -> name dep == rname) (allDeps)
