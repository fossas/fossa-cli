module Strategy.Python.PDM.PdmLock (
  PdmLock (..),
  PdmLockPackage (..),
  pdmLockCodec,
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
import Strategy.Python.Util (Req (..), reqCodec)
import Toml (TomlCodec, (.=))
import Toml qualified

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

pdmLockCodec :: TomlCodec PdmLock
pdmLockCodec = PdmLock <$> Toml.list pdmLockPackageCodec "package" .= pdmLockPackages

pdmLockPackageCodec :: TomlCodec PdmLockPackage
pdmLockPackageCodec =
  PdmLockPackage
    <$> Toml.diwrap (Toml.text "name") .= name
    <*> Toml.text "version" .= version
    <*> Toml.dioptional (Toml.text "git") .= gitUrl
    <*> Toml.dioptional (Toml.text "revision") .= gitRevision
    <*> Toml.dioptional (Toml.text "path") .= fsPath
    <*> Toml.dioptional (Toml.text "url") .= url
    <*> Toml.dioptional (Toml.arrayOf reqCodec "dependencies") .= pdmDependencies

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
    depName = case (gitUrl pkg, url pkg) of
      (Just url, _) -> url
      (_, Just u) -> u
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
