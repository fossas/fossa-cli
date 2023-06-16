module Graphing.Enrich (
  pathToArchives,
) where

import App.Fossa.LicenseScanner (licenseScanSourceUnit')
import App.Fossa.VendoredDependency (VendoredDependency (..), VendoredDependencyScanMode (SkipPreviouslyScanned))
import App.Types (FullFileUploads (..))
import Control.Algebra (Has)
import Control.Carrier.Lift (Lift)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, fatal)
import Control.Effect.FossaApiClient (FossaApiClient)
import Control.Effect.StickyLogger (StickyLogger)
import Control.Monad (unless)
import Data.Either (partitionEithers)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map, fromList)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.String.Conversion (toText)
import Data.Text (Text, intercalate)
import Data.Text.Extra (splitOnceOn)
import DepTypes (DepEnvironment, DepType (ArchiveType, PathType), Dependency (..), VerConstraint (CEq))
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Effect.Exec (Exec)
import Effect.Logger (Logger, indent, pretty, vsep)
import Effect.ReadFS (ReadFS)
import Graphing (Graphing, gmap, vertexList)
import Path (Abs, Dir, Path)
import Srclib.Converter (fetcherToDepType, toLocator)
import Srclib.Types (Locator (locatorFetcher, locatorProject, locatorRevision))

-- | Transform all 'Path' dependency to 'Archive' dependency.
-- It performs cli-side license scanning and upload of results. In this,
-- transformation,
-- -
-- * It preserves edges of path dependencies in the transformation.
-- * It DOES NOT ignores the non-production dependency in transformation.
-- * It DOES NOT care about version constraint provided on path dependency,
-- because, it always performs scan and upload. It does this to avoid scenario,
-- where, multiple project may have path+dep$a, but different underlying content.
pathToArchives ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has FossaApiClient sig m
  ) =>
  Path Abs Dir -> -- directory of the project's manifest
  Graphing Dependency -> -- graph whose path dependency to make archives
  m (Graphing Dependency) -- graph with archive dependencies
pathToArchives rootPath g =
  -- If we do not have any path dependencies, we do not
  -- need to do any work!
  case (null unableToTransformPathDep, transformedVendorDep) of
    (True, Nothing) -> pure g
    (False, _) -> fatal $ FailedToTransformPathDependency unableToTransformPathDep
    (True, Just depsAndVendorDeps) -> do
      let vendorDeps = NE.map snd depsAndVendorDeps
      let vendorDepToOriginalDep = Map.fromList . map flipTuple . NE.toList $ depsAndVendorDeps

      -- 1. We upload all vendor deps (transformed from path deps),
      -- since path dependencies have to be scanned
      -- before they can be referenced in graph as archive deps!
      vendorDepAndLocators <-
        licenseScanSourceUnit'
          SkipPreviouslyScanned
          Nothing
          (FullFileUploads False)
          rootPath
          vendorDeps

      -- 2. We replace all path dependencies with
      -- archive dependency by transforming archive locators to
      -- archive dependency
      let (unableToTransformLocators, archiveDeps) =
            partitionEithers $
              map (uncurry (vendorDepToDep vendorDepToOriginalDep)) $
                toList (vendorDepAndLocators)

      unless (null unableToTransformLocators) $
        fatal $
          FailedToTransformLocators unableToTransformLocators

      -- 3. Now we replace all archive deps with path deps
      -- in original graph. If we are unable to find twin of archive dep
      -- (i.e. path dep), we fail fatally!
      case fromList <$> archiveToPathDep archiveDeps of
        Left singleArchiveDeps -> fatal $ UnableToFindTwinOfArchiveDep singleArchiveDeps
        Right registry -> pure $ gmap (\graphDep -> Map.findWithDefault graphDep graphDep registry) g
  where
    allDeps :: [Dependency]
    allDeps = vertexList g

    allPathDeps :: [Dependency]
    allPathDeps = filter isPathDep allDeps

    unableToTransformPathDep :: [Dependency]
    unableToTransformPathDep = fst $ toVendorDeps allPathDeps

    transformedVendorDep :: Maybe (NonEmpty (Dependency, VendoredDependency))
    transformedVendorDep = nonEmpty . snd $ toVendorDeps allPathDeps

    archiveToPathDep :: [Dependency] -> Either [Dependency] [(Dependency, Dependency)]
    archiveToPathDep archiveDeps = sequence' $ map (findArchiveTwin archiveDeps) allPathDeps

sequence' :: [Either a b] -> Either [a] [b]
sequence' r = case partitionEithers r of
  ([], xs) -> Right xs
  (xs, _) -> Left xs

toVendorDeps :: [Dependency] -> ([Dependency], [(Dependency, VendoredDependency)])
toVendorDeps deps = partitionEithers $ map pathDepToVendorDep deps

-- | Find's archive twin for given path dependency.
-- -
-- In this we disregard, path dependency's version. Only criteria we care about
-- is name, and location as provided in the dependency metadata. Since, we are always
-- using hash instead of version (of path dependency as seen from manifest), this is
-- satisfactory solution.
-- -
-- Example:
-- >> findArchiveTwin ["archive+1/twinkle"] -> path+twinkle$2 -> Right ("path+twinkle$2", "archive+1/twinkle")
-- >> findArchiveTwin ["archive+1/star"] -> path+twinkle$2 -> Left "path+twinkle$2"
findArchiveTwin :: [Dependency] -> Dependency -> Either Dependency (Dependency, Dependency)
findArchiveTwin archiveDeps pathDep = case find
  ( \d ->
      (unOrg . dependencyName $ d) == (dependencyName pathDep)
        && dependencyType d == ArchiveType
        && dependencyType pathDep == PathType
        && dependencyLocations d == dependencyLocations pathDep
  )
  archiveDeps of
  Just d' -> Right (pathDep, d')
  _ -> Left pathDep
  where
    unOrg :: Text -> Text
    unOrg t = snd $ splitOnceOn "/" t

-- | True if dependency is path dependency
isPathDep :: Dependency -> Bool
isPathDep d = dependencyType d == PathType

-- | Trasnforms Path dependency to Vendor Dependency.
pathDepToVendorDep :: Dependency -> Either Dependency (Dependency, VendoredDependency)
pathDepToVendorDep d =
  case listToMaybe $ dependencyLocations d of
    Nothing -> Left d
    Just p -> Right (d, VendoredDependency (dependencyName d) p Nothing)

-- | Transforms Vendor dependency to Dependency using locator, if possible.
vendorDepToDep ::
  Map VendoredDependency Dependency ->
  VendoredDependency ->
  Locator ->
  Either Locator Dependency
vendorDepToDep vendorToDep vd l = case fetcherToDepType $ locatorFetcher l of
  Just ArchiveType ->
    Right $
      Dependency
        ArchiveType
        (locatorProject l)
        (CEq <$> locatorRevision l)
        [vendoredPath vd]
        envs
        tags
  Nothing -> Left l
  Just _ -> Left l
  where
    envs :: Set DepEnvironment
    envs = maybe mempty dependencyEnvironments (Map.lookup vd' vendorToDep)

    tags :: Map Text [Text]
    tags = maybe mempty dependencyTags (Map.lookup vd' vendorToDep)

    vd' :: VendoredDependency
    vd' = vd{vendoredVersion = Nothing}

renderDep :: Dependency -> Text
renderDep d =
  toText (toLocator d)
    <> " at: "
    <> intercalate "," (dependencyLocations d)

flipTuple :: (a, b) -> (b, a)
flipTuple (a, b) = (b, a)

newtype FailedToTransformPathDependency = FailedToTransformPathDependency [Dependency]
instance ToDiagnostic FailedToTransformPathDependency where
  renderDiagnostic (FailedToTransformPathDependency deps) =
    vsep
      [ "We could not transform analyzed path like dependency to vendored dependency."
      , indent 2 $ vsep $ map (pretty . renderDep) deps
      , indent 2 $
          vsep
            [ "Ensure location is provided for path like dependency."
            ]
      ]

newtype FailedToTransformLocators = FailedToTransformLocators [Locator]
instance ToDiagnostic FailedToTransformLocators where
  renderDiagnostic (FailedToTransformLocators locs) =
    vsep
      [ "We could not transform vendored dependency to archive dependency."
      , indent 2 $ vsep $ map (pretty . toText) locs
      , indent 2 $
          vsep
            [ "This is likely a defect, please contact FOSSA support!"
            ]
      ]

newtype UnableToFindTwinOfArchiveDep = UnableToFindTwinOfArchiveDep [Dependency]
instance ToDiagnostic UnableToFindTwinOfArchiveDep where
  renderDiagnostic (UnableToFindTwinOfArchiveDep deps) =
    vsep
      [ "We could not identify path dependency for following archive dependency."
      , indent 2 $ vsep $ map (pretty . toText . toLocator) deps
      , indent 2 $
          vsep
            [ "This is likely a defect, please contact FOSSA support!"
            ]
      ]
