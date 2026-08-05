-- | Convert Go binaries discovered by millhone in container image layers into
-- source units carrying real @go+@ locators, so the backend resolves
-- licenses and vulnerabilities exactly as it does for go.mod analysis.
module App.Fossa.Container.Sources.GoBinary (
  goBinariesToSourceUnits,
  goBinaryToSourceUnit,
  goModuleToLocator,
) where

import Container.Types (DiscoveredGoBinary (..), GoModule (..))
import Data.Aeson qualified as Aeson
import Data.List (nub)
import Data.Maybe (mapMaybe, maybeToList)
import Data.SemVer qualified as SemVer
import Data.SemVer.Internal (Version (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Srclib.Types (Locator (..), SourceUnit (..), SourceUnitBuild (..), SourceUnitDependency (..), textToOriginPath)
import Strategy.Go.Gomod (PackageVersion (..), parsePackageVersion)
import Text.Megaparsec (parseMaybe)
import Types (GraphBreadth (..))

-- | One source unit per discovered binary. Binaries with no usable module
-- versions (e.g. only a "(devel)" main module) produce no unit at all.
goBinariesToSourceUnits :: [DiscoveredGoBinary] -> [SourceUnit]
goBinariesToSourceUnits = mapMaybe goBinaryToSourceUnit

goBinaryToSourceUnit :: DiscoveredGoBinary -> Maybe SourceUnit
goBinaryToSourceUnit binary =
  case locators of
    [] -> Nothing
    _ ->
      Just
        SourceUnit
          { sourceUnitName = goBinaryPath binary
          , sourceUnitType = "gobinary"
          , sourceUnitManifest = goBinaryPath binary
          , sourceUnitBuild =
              Just
                SourceUnitBuild
                  { buildArtifact = "default"
                  , buildSucceeded = True
                  , buildImports = locators
                  , buildDependencies = map toSourceUnitDependency locators
                  }
          , -- Buildinfo lists every module linked into the binary.
            sourceUnitGraphBreadth = Complete
          , sourceUnitNoticeFiles = []
          , sourceUnitOriginPaths = [textToOriginPath $ goBinaryPath binary]
          , sourceUnitLabels = Nothing
          , additionalData = Nothing
          }
  where
    -- The main module is normally versioned "(devel)" and dropped, but
    -- binaries built via @go install module\@version@ carry a real version.
    candidates :: [GoModule]
    candidates = goBinaryModules binary <> maybeToList (goBinaryMainModule binary)

    locators :: [Locator]
    locators = nub $ mapMaybe goModuleToLocator candidates

    toSourceUnitDependency :: Locator -> SourceUnitDependency
    toSourceUnitDependency locator = SourceUnitDependency locator [] Aeson.Null

-- | Build a @go+@ locator, normalizing the version the same way go.mod
-- analysis does ('Strategy.Go.GoListPackages.toVerConstraint'): pseudo-versions
-- become their commit hash, semantic versions keep their "v" prefix.
-- Unusable versions (empty, "(devel)") yield 'Nothing'.
goModuleToLocator :: GoModule -> Maybe Locator
goModuleToLocator (GoModule path version) = do
  normalized <- normalizeVersion version
  Just
    Locator
      { locatorFetcher = "go"
      , locatorProject = path
      , locatorRevision = Just normalized
      }

normalizeVersion :: Text -> Maybe Text
normalizeVersion version
  | Text.null version || version == "(devel)" = Nothing
  | otherwise = case parseMaybe (parsePackageVersion id) version of
      Just (Pseudo commitHash) -> Just commitHash
      Just (Semantic semver) -> Just ("v" <> SemVer.toText semver{_versionMeta = []})
      Just (NonCanonical v) -> Just v
      Nothing -> Just version
