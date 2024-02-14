{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Strategy.Node (
  discover,
  pkgGraph,
  NodeProject (..),
  getDeps,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Extra qualified as AME
import App.Fossa.Analyze.LicenseAnalyze (LicenseAnalyzeProject, licenseAnalyzeProject)
import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject, analyzeProjectStaticOnly))
import Control.Carrier.Diagnostics (errDoc)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  errCtx,
  errHelp,
  fatalText,
  fromEitherShow,
  fromMaybe,
  recover,
  warnOnErr,
 )
import Control.Effect.Reader (Reader)
import Control.Monad (void, (<=<))
import Data.Glob (Glob)
import Data.Glob qualified as Glob
import Data.List.Extra (singleton)
import Data.Map (Map, toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (decodeUtf8)
import Data.Tagged (applyTag)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml.Aeson (ToJSON)
import Diag.Common (
  MissingDeepDeps (MissingDeepDeps),
  MissingEdges (MissingEdges),
 )
import Discovery.Filters (AllFilters, withMultiToolFilter)
import Discovery.Walk (
  WalkStep (WalkSkipSome),
  findFileNamed,
  walk',
 )
import Effect.Logger (
  Logger,
 )
import Effect.ReadFS (
  ReadFS,
  doesFileExist,
  readContentsBSLimit,
  readContentsJson,
 )
import GHC.Generics (Generic)
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  mkRelFile,
  parent,
  toFilePath,
  (</>),
 )
import Strategy.Node.Errors (CyclicPackageJson (CyclicPackageJson), MissingNodeLockFile (..), fossaNodeDocUrl, npmLockFileDocUrl, yarnLockfileDocUrl, yarnV2LockfileDocUrl)
import Strategy.Node.Npm.PackageLock qualified as PackageLock
import Strategy.Node.Npm.PackageLockV3 qualified as PackageLockV3
import Strategy.Node.PackageJson (
  Development,
  FlatDeps (FlatDeps),
  Manifest (..),
  NodePackage (NodePackage),
  PackageJson (..),
  PkgJsonGraph (..),
  PkgJsonLicense (LicenseObj, LicenseText),
  PkgJsonLicenseObj (licenseUrl),
  PkgJsonWorkspaces (unWorkspaces),
  Production,
  WorkspacePackageNames (WorkspacePackageNames),
  pkgFileList,
 )
import Strategy.Node.PackageJson qualified as PackageJson
import Strategy.Node.Pnpm.PnpmLock qualified as PnpmLock
import Strategy.Node.YarnV1.YarnLock qualified as V1
import Strategy.Node.YarnV2.YarnLock qualified as V2
import Types (
  DependencyResults (DependencyResults),
  DiscoveredProject (..),
  DiscoveredProjectType (NpmProjectType, PnpmProjectType, YarnProjectType),
  FoundTargets (ProjectWithoutTargets),
  GraphBreadth (Complete, Partial),
  License (License),
  LicenseResult (LicenseResult, licensesFound),
  LicenseType (LicenseURL, UnknownType),
  licenseFile,
 )

skipJsFolders :: WalkStep
skipJsFolders = WalkSkipSome ["node_modules", "bower_components", ".yarn"]

discover ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject NodeProject]
discover dir = withMultiToolFilter [YarnProjectType, NpmProjectType, PnpmProjectType] $
  context "NodeJS" $ do
    manifestList <- context "Finding nodejs projects" $ collectManifests dir
    manifestMap <- context "Reading package.json files" $ (Map.fromList . catMaybes) <$> traverse loadPackage manifestList
    if Map.null manifestMap
      then -- If the map is empty, we found no JS projects, we return early.
        pure []
      else do
        globalGraph <- context "Building global workspace graph" $ pure $ buildManifestGraph manifestMap
        -- TODO: refactor splitGraph to report which cycle we hit, not just report some unknown cycle
        graphs <- context "Splitting global graph into chunks" $ fromMaybe CyclicPackageJson $ splitGraph globalGraph
        context "Converting graphs to analysis targets" $ traverse (mkProject <=< identifyProjectType) graphs

collectManifests :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [Manifest]
collectManifests = walk' $ \_ _ files ->
  case findFileNamed "package.json" files of
    Nothing -> pure ([], skipJsFolders)
    Just jsonFile -> pure ([Manifest jsonFile], skipJsFolders)

mkProject ::
  ( Has Diagnostics sig m
  ) =>
  NodeProject ->
  m (DiscoveredProject NodeProject)
mkProject project = do
  let (graph, typename) = case project of
        Yarn _ g -> (g, YarnProjectType)
        NPMLock _ g -> (g, NpmProjectType)
        NPM g -> (g, NpmProjectType)
        Pnpm _ g -> (g, PnpmProjectType)
  Manifest rootManifest <- fromEitherShow $ findWorkspaceRootManifest graph
  pure $
    DiscoveredProject
      { projectType = typename
      , projectPath = parent rootManifest
      , projectBuildTargets = ProjectWithoutTargets
      , projectData = project
      }

instance AnalyzeProject NodeProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDeps

-- Since we don't natively support workspaces, we don't attempt to preserve them from this point on.
-- In the future, if you're adding generalized workspace support, start here.
getDeps ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  NodeProject ->
  m DependencyResults
getDeps (Yarn yarnLockFile graph) = analyzeYarn yarnLockFile graph
getDeps (NPMLock packageLockFile graph) = analyzeNpmLock packageLockFile graph
getDeps (Pnpm pnpmLockFile _) = analyzePnpmLock pnpmLockFile
getDeps (NPM graph) = analyzeNpm graph

analyzePnpmLock :: (Has Diagnostics sig m, Has ReadFS sig m, Has Logger sig m) => Manifest -> m DependencyResults
analyzePnpmLock (Manifest pnpmLockFile) = do
  result <- PnpmLock.analyze pnpmLockFile
  pure $ DependencyResults result Complete [pnpmLockFile]

analyzeNpmLock :: (Has Diagnostics sig m, Has ReadFS sig m) => Manifest -> PkgJsonGraph -> m DependencyResults
analyzeNpmLock (Manifest npmLockFile) graph = do
  npmLockVersion <- detectNpmLockVersion npmLockFile
  result <- case npmLockVersion of
    NpmLockV3Compatible -> PackageLockV3.analyze npmLockFile
    NpmLockV1Compatible -> PackageLock.analyze npmLockFile (extractDepLists graph) (findWorkspaceNames graph)
  pure $ DependencyResults result Complete [npmLockFile]

analyzeNpm :: (Has Diagnostics sig m) => PkgJsonGraph -> m DependencyResults
analyzeNpm wsGraph = do
  void
    . recover
    . warnOnErr MissingEdges
    . warnOnErr MissingDeepDeps
    . errCtx MissingNodeLockFileCtx
    . errHelp MissingNodeLockFileHelp
    . errDoc fossaNodeDocUrl
    . errDoc npmLockFileDocUrl
    . errDoc yarnLockfileDocUrl
    . errDoc yarnV2LockfileDocUrl
    $ fatalText "Lock files - yarn.lock or package-lock.json were not discovered."

  graph <- PackageJson.analyze $ Map.elems $ jsonLookup wsGraph
  pure $ DependencyResults graph Partial $ pkgFileList wsGraph

analyzeYarn ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Manifest ->
  PkgJsonGraph ->
  m DependencyResults
analyzeYarn (Manifest yarnLockFile) pkgJsonGraph = do
  yarnVersion <- detectYarnVersion yarnLockFile
  let analyzeFunc = case yarnVersion of
        V1 -> V1.analyze
        V2Compatible -> V2.analyze

  graph <- analyzeFunc yarnLockFile $ extractDepLists pkgJsonGraph
  pure . DependencyResults graph Complete $ yarnLockFile : pkgFileList pkgJsonGraph

detectYarnVersion ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Path Abs File ->
  m YarnVersion
detectYarnVersion yarnfile = do
  -- we expect the v1 header to end at char 82
  contents <- decodeUtf8 <$> readContentsBSLimit yarnfile 100
  if "yarn lockfile v1" `Text.isInfixOf` contents
    then pure V1
    else pure V2Compatible

data YarnVersion
  = V1
  | V2Compatible

data NpmLockVersion
  = NpmLockV1Compatible
  | NpmLockV3Compatible

detectNpmLockVersion ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Path Abs File ->
  m NpmLockVersion
detectNpmLockVersion npmLockFile = do
  isV3Compatible <- recover $ readContentsJson @PackageLockV3.PackageLockV3 npmLockFile
  if isJust isV3Compatible
    then pure NpmLockV3Compatible
    else pure NpmLockV1Compatible

-- | Find every manifest that is a child of some other package and look
-- up their @packageName@ in the @jsonLookup@ map.
findWorkspaceNames :: PkgJsonGraph -> WorkspacePackageNames
findWorkspaceNames PkgJsonGraph{..} =
  WorkspacePackageNames
    . Set.fromList
    $ workspaceNames
  where
    childManifests :: [Manifest]
    childManifests = map snd . AM.edgeList $ jsonGraph

    workspaceNames :: [Text]
    workspaceNames = mapMaybe (packageName <=< flip Map.lookup jsonLookup) childManifests

extractDepLists :: PkgJsonGraph -> FlatDeps
extractDepLists PkgJsonGraph{..} = foldMap extractSingle $ Map.elems jsonLookup
  where
    mapToSet :: Map Text Text -> Set NodePackage
    mapToSet = Set.fromList . map (uncurry NodePackage) . Map.toList

    extractSingle :: PackageJson -> FlatDeps
    extractSingle PackageJson{..} =
      FlatDeps
        (applyTag @Production $ mapToSet (packageDeps `Map.union` packagePeerDeps))
        (applyTag @Development $ mapToSet packageDevDeps)
        (Map.keysSet jsonLookup)

loadPackage :: (Has Logger sig m, Has ReadFS sig m, Has Diagnostics sig m) => Manifest -> m (Maybe (Manifest, PackageJson))
loadPackage (Manifest file) = do
  result <- recover $ readContentsJson @PackageJson file
  case result of
    Nothing -> pure Nothing
    Just contents -> pure $ Just (Manifest file, contents)

buildManifestGraph :: Map Manifest PackageJson -> PkgJsonGraph
buildManifestGraph manifestMap = PkgJsonGraph adjmap manifestMap
  where
    -- Run 'go' on each key/value pair: (file path, parsed contents of that file)
    adjmap :: AM.AdjacencyMap Manifest
    adjmap =
      manifestVertices
        `AM.overlay` Map.foldrWithKey
          (\k v m -> AM.overlay m $ go k v)
          AM.empty
          manifestMap

    -- Make sure all manifests end up in the graph, ignoring workspaces
    manifestVertices :: AM.AdjacencyMap Manifest
    manifestVertices = AM.vertices $ Map.keys manifestMap

    -- For a single package.json, find all direct children (in terms of workspaces).
    go :: Manifest -> PackageJson -> AM.AdjacencyMap Manifest
    go path pkgJson =
      foldr (\g m -> AM.overlay m $ findWorkspaceChildren path g) AM.empty
        . unWorkspaces
        $ packageWorkspaces pkgJson

    -- Given a workspace pattern, find all matches in the list of known manifest files.
    -- When found, create edges between the root path and the matching children.
    findWorkspaceChildren :: Manifest -> Glob Rel -> AM.AdjacencyMap Manifest
    findWorkspaceChildren path glob =
      manifestEdges path . filter (filterfunc path glob) $
        Map.keys manifestMap

    -- True if qualified glob pattern matches the given file.
    filterfunc :: Manifest -> Glob Rel -> Manifest -> Bool
    filterfunc root glob (Manifest candidate) = candidate `Glob.matches` qualifyGlobPattern root glob

    -- Yarn appends the filename to the glob, so we match that behavior
    -- https://github.com/yarnpkg/yarn/blob/master/src/config.js#L821
    qualifyGlobPattern :: Manifest -> Glob Rel -> Glob Abs
    qualifyGlobPattern (Manifest root) = Glob.append "package.json" . Glob.prefixWith (parent root)

    -- Create edges from a parent to its children
    manifestEdges :: Ord a => a -> [a] -> AM.AdjacencyMap a
    manifestEdges path children = AM.edges $ map (path,) children

splitGraph :: PkgJsonGraph -> Maybe [PkgJsonGraph]
splitGraph PkgJsonGraph{..} = map (splitFromParent) <$> AME.splitGraph jsonGraph
  where
    splitFromParent :: AM.AdjacencyMap Manifest -> PkgJsonGraph
    splitFromParent graph = PkgJsonGraph graph $ extractMapChunk graph jsonLookup

    extractMapChunk :: Ord k => AM.AdjacencyMap k -> Map k a -> Map k a
    extractMapChunk graph bigmap = Map.fromList . mapMaybe (getpair bigmap) $ AM.vertexList graph

    getpair :: Ord k => Map k a -> k -> Maybe (k, a)
    getpair mapping k = do
      val <- mapping Map.!? k
      pure (k, val)

identifyProjectType ::
  ( Has Logger sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  PkgJsonGraph ->
  m NodeProject
identifyProjectType graph = do
  Manifest manifest <- fromEitherShow $ findWorkspaceRootManifest graph
  let yarnFilePath = parent manifest Path.</> $(mkRelFile "yarn.lock")
      packageLockPath = parent manifest Path.</> $(mkRelFile "package-lock.json")
      pnpmLockPath = parent manifest Path.</> $(mkRelFile "pnpm-lock.yaml")
  yarnExists <- doesFileExist yarnFilePath
  pkgLockExists <- doesFileExist packageLockPath
  pnpmLockExists <- doesFileExist pnpmLockPath
  pure $ case (yarnExists, pkgLockExists, pnpmLockExists) of
    (True, _, _) -> Yarn (Manifest yarnFilePath) graph
    (_, True, _) -> NPMLock (Manifest packageLockPath) graph
    (_, _, True) -> Pnpm (Manifest pnpmLockPath) graph
    _ -> NPM graph

data NodeProject
  = Yarn Manifest PkgJsonGraph
  | NPMLock Manifest PkgJsonGraph
  | NPM PkgJsonGraph
  | Pnpm Manifest PkgJsonGraph
  deriving (Eq, Ord, Show, Generic)

instance LicenseAnalyzeProject NodeProject where
  licenseAnalyzeProject = pure . analyzeLicenses . pkgGraph

analyzeLicenses :: PkgJsonGraph -> [LicenseResult]
analyzeLicenses (PkgJsonGraph _ graph) = mapMaybe (uncurry mkLicenseResult) . toList $ graph

mkLicenseResult :: Manifest -> PackageJson -> Maybe LicenseResult
mkLicenseResult manifest PackageJson{..} = constrLicenseResult <$> allLicenses
  where
    manifestPath = toFilePath . unManifest $ manifest
    allLicenses =
      (singleton <$> packageLicense)
        <> (map LicenseObj <$> packageLicenses)

    mkLicense (LicenseText txt) = License UnknownType txt
    mkLicense (LicenseObj pjlo) = License LicenseURL (licenseUrl pjlo)

    constrLicenseResult licenses =
      LicenseResult
        { licenseFile = manifestPath
        , licensesFound = map mkLicense licenses
        }

instance ToJSON NodeProject

pkgGraph :: NodeProject -> PkgJsonGraph
pkgGraph (Yarn _ pjg) = pjg
pkgGraph (NPMLock _ pjg) = pjg
pkgGraph (NPM pjg) = pjg
pkgGraph (Pnpm _ pjg) = pjg

findWorkspaceRootManifest :: PkgJsonGraph -> Either String Manifest
findWorkspaceRootManifest PkgJsonGraph{jsonGraph} =
  case AM.vertexList $ AM.induce (hasNoIncomingEdges jsonGraph) jsonGraph of
    [x] -> Right x
    _ -> Left "package.json workspace graph must have exactly 1 root manifest"

hasNoIncomingEdges :: Ord a => AM.AdjacencyMap a -> a -> Bool
hasNoIncomingEdges graph item = Set.null $ AM.preSet item graph
