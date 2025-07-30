module Strategy.Conan.Enrich (
  conanToArchives,
  -- for testing
  conanDepToVendorDep,
  locatorToArchiveDep,
  findArchiveTwin,
) where

import App.Fossa.LicenseScanner (licenseScanSourceUnit)
import App.Fossa.VendoredDependency (VendoredDependency (..), VendoredDependencyScanMode (SkippingNotSupported))
import App.Types (FileUpload (..))
import Control.Algebra (Has)
import Control.Carrier.Lift (Lift)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, errHelp, errSupport, fatal)
import Control.Effect.FossaApiClient (FossaApiClient)
import Control.Effect.StickyLogger (StickyLogger)
import Control.Monad (unless)
import Data.Either (partitionEithers)
import Data.Error (SourceLocation, createEmptyBlock, getSourceLocation)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Data.List.NonEmpty qualified as NE
import Data.Map (fromList)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text, intercalate)
import Data.Text.Extra (splitOnceOn)
import DepTypes (DepType (ArchiveType, ConanType), Dependency (..), VerConstraint (CEq))
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Effect.Exec (Exec)
import Effect.Logger (Logger, indent, pretty, renderIt, vsep)
import Effect.ReadFS (ReadFS)
import Errata (Errata, errataSimple)
import Graphing (Graphing, gmap, vertexList)
import Path (Abs, Dir, Path)
import Srclib.Converter (fetcherToDepType, toLocator, verConstraintToRevision)
import Srclib.Types (Locator (locatorFetcher, locatorProject, locatorRevision))

newtype LonelyDeps = LonelyDeps {unLonelyDeps :: [Dependency]}
  deriving (Eq, Ord, Show)

-- | Transform all 'Conan' dependency to 'Archive' dependency.
-- It performs cli-side license scanning and upload of results. In this,
-- transformation,
-- -
-- * It preserves edges of conan dependencies in the transformation.
-- * TODO: Ignore non-production dependency in transformation
conanToArchives ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has FossaApiClient sig m
  ) =>
  Path Abs Dir -> -- directory of the project's manifest
  FileUpload ->
  Graphing Dependency -> -- graph whose conan dependency to make archives
  m (Graphing Dependency) -- graph with archive dependencies
conanToArchives rootPath uploadKind g =
  -- If we do not have any conan dependencies, we do not
  -- need to do any work!
  case (null unableToTransformConanDep, transformedVendorDep) of
    (True, Nothing) -> pure g
    (False, _) -> errHelp ("Ensure location is provided for conan dependency" :: Text) $ fatal $ FailedToTransformConanDependency getSourceLocation unableToTransformConanDep
    (True, Just depsAndVendorDeps) -> do
      let vendorDeps = NE.map snd depsAndVendorDeps

      -- 1. We upload all vendor deps (transformed from conan deps),
      -- since conan dependencies have to be scanned
      -- before they can be referenced in graph as archive deps!
      archiveLocators <-
        licenseScanSourceUnit
          -- always perform scan as vendor deps's
          -- manifest are rarely updated with changes in practice
          SkippingNotSupported
          Nothing -- Ignore path filters
          uploadKind
          rootPath
          vendorDeps

      -- 2. We make archive archive dependencies from locators, and original
      -- conan dependencies.
      let (failed, archiveDeps) =
            partitionEithers $
              map (locatorToArchiveDep allConanDeps) $
                toList archiveLocators

      unless (null failed) $
        errSupport enrichSupportMessage $
          fatal $
            FailedToTransformLocators getSourceLocation failed

      -- 3. We replace all conan dependencies with archive dependencies from
      -- original graph. If we are unable to find twin of archive dep
      -- (e.g. sourcing conan dep), we fail fatally!
      case fromList <$> archiveToConanDep archiveDeps of
        Left lonelyArchiveDeps -> errSupport enrichSupportMessage $ fatal $ UnableToFindTwinOfArchiveDep getSourceLocation lonelyArchiveDeps
        Right registry -> pure $ gmap (\graphDep -> Map.findWithDefault graphDep graphDep registry) g
  where
    allDeps :: [Dependency]
    allDeps = vertexList g

    allConanDeps :: [Dependency]
    allConanDeps = filter isConanDep allDeps

    unableToTransformConanDep :: [Dependency]
    unableToTransformConanDep = fst $ toVendorDeps allConanDeps

    transformedVendorDep :: Maybe (NonEmpty (Dependency, VendoredDependency))
    transformedVendorDep = nonEmpty . snd $ toVendorDeps allConanDeps

    archiveToConanDep :: [Dependency] -> Either LonelyDeps [(Dependency, Dependency)]
    archiveToConanDep archiveDeps = partitionEithers' $ map (findArchiveTwin archiveDeps) allConanDeps

-- | True if dependency is a conan dependency, otherwise False.
isConanDep :: Dependency -> Bool
isConanDep d = dependencyType d == ConanType

partitionEithers' :: [Either Dependency a] -> Either LonelyDeps [a]
partitionEithers' r = case partitionEithers r of
  ([], xs) -> Right xs
  (xs, _) -> Left $ LonelyDeps xs

toVendorDeps :: [Dependency] -> ([Dependency], [(Dependency, VendoredDependency)])
toVendorDeps deps = partitionEithers $ map conanDepToVendorDep deps

-- | Transforms Conan dependency to Vendor Dependency.
conanDepToVendorDep :: Dependency -> Either Dependency (Dependency, VendoredDependency)
conanDepToVendorDep d =
  case listToMaybe $ dependencyLocations d of
    Nothing -> Left d
    Just pathOfDep -> Right (d, VendoredDependency depName pathOfDep depVersion Nothing [])
  where
    depName :: Text
    depName = dependencyName d

    -- Conan dependencies are expected to have version with package identifier hash
    -- which is unique to build system, and build configuration.
    depVersion :: Maybe Text
    depVersion = verConstraintToRevision =<< dependencyVersion d

-- | Transforms Vendored dependency into Archive Dependency using locator, if possible.
-- Locator must be of archive kind, and archive locator's project and revision must exist
-- within provided deps.
-- -
-- >> vendorDepToDep ["conan+little$1"] "archive+42/little$1" = Right "archive+42/little$1"
-- >> vendorDepToDep ["conan+star$1"] "archive+42/little$1" = Left "archive+42/little$1"
locatorToArchiveDep ::
  [Dependency] ->
  Locator ->
  Either Locator Dependency
locatorToArchiveDep deps l = case (fetcherToDepType $ locatorFetcher l, depOfLocator) of
  (Just ArchiveType, Just originalDep) ->
    Right $
      Dependency
        ArchiveType
        (locatorProject l)
        (CEq <$> locatorRevision l)
        (dependencyLocations originalDep)
        (dependencyEnvironments originalDep)
        (dependencyTags originalDep)
  _ -> Left l
  where
    depOfLocator :: Maybe Dependency
    depOfLocator =
      find
        ( \d ->
            dependencyName d == (unOrg . locatorProject $ l)
              && isConanDep d
              && dependencyVersion d == (CEq <$> locatorRevision l)
        )
        deps

-- | Find's archive twin for given conan dependency.
-- -
-- In this we disregard, conan dependency's version. Only criteria we care about
-- is name, and location as provided in the dependency metadata. Since, we are always
-- using hash instead of version (of conan dependency as seen from manifest), this is
-- satisfactory solution.
-- -
-- Example:
-- >> findArchiveTwin ["archive+1/twinkle"] -> conan+twinkle$2 -> Right ("conan+twinkle$2", "archive+1/twinkle")
-- >> findArchiveTwin ["archive+1/star"] -> conan+twinkle$2 -> Left "conan+twinkle$2"
findArchiveTwin :: [Dependency] -> Dependency -> Either Dependency (Dependency, Dependency)
findArchiveTwin archiveDeps conanDep = case find
  ( \d ->
      (unOrg . dependencyName $ d) == (dependencyName conanDep)
        && dependencyType d == ArchiveType
        && dependencyType conanDep == ConanType
        && dependencyLocations d == dependencyLocations conanDep
  )
  archiveDeps of
  Just archiveDep -> Right (conanDep, archiveDep)
  _ -> Left conanDep

-- | Removes org identifier from text
-- >> unOrg "42/name" = "name"
unOrg :: Text -> Text
unOrg t = snd $ splitOnceOn "/" t

enrichSupportMessage :: Text
enrichSupportMessage = "This is likely a defect, please contact FOSSA support at: https://support.fossa.com/"

data FailedToTransformConanDependency = FailedToTransformConanDependency SourceLocation [Dependency]
instance ToDiagnostic FailedToTransformConanDependency where
  renderDiagnostic :: FailedToTransformConanDependency -> Errata
  renderDiagnostic (FailedToTransformConanDependency srcLoc deps) = do
    let body =
          renderIt $
            vsep
              [indent 2 $ vsep $ map (pretty . renderDep) deps]
    errataSimple (Just "Could not transform analyzed conan dependency to vendored dependency") (createEmptyBlock srcLoc) (Just body)
    where
      renderDep :: Dependency -> Text
      renderDep d =
        toText (toLocator d)
          <> " at: "
          <> intercalate "," (dependencyLocations d)

data FailedToTransformLocators = FailedToTransformLocators SourceLocation [Locator]
instance ToDiagnostic FailedToTransformLocators where
  renderDiagnostic :: FailedToTransformLocators -> Errata
  renderDiagnostic (FailedToTransformLocators srcLoc locs) = do
    let body =
          renderIt $
            vsep
              [vsep $ map (pretty . toText) locs]
    errataSimple (Just "Could not transform vendored dependency to archive dependency") (createEmptyBlock srcLoc) (Just body)

data UnableToFindTwinOfArchiveDep = UnableToFindTwinOfArchiveDep SourceLocation LonelyDeps
instance ToDiagnostic UnableToFindTwinOfArchiveDep where
  renderDiagnostic :: UnableToFindTwinOfArchiveDep -> Errata
  renderDiagnostic (UnableToFindTwinOfArchiveDep srcLoc (LonelyDeps deps)) = do
    let body =
          renderIt $
            vsep
              [ "We could not identify conan dependency for following dependencies:"
              , indent 2 $ vsep $ map (pretty . toText . toLocator) deps
              ]
    errataSimple (Just "Could not identify conan dependency") (createEmptyBlock srcLoc) (Just body)
