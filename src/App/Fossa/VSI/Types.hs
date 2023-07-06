{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSI.Types (
  ScanID (..),
  Locator (..),
  AnalysisStatus (..),
  LocatorParseError (..),
  SkipResolution (..),
  shouldSkipResolving,
  parseLocator,
  renderLocator,
  isUserDefined,
  userDefinedFetcher,
  isTopLevelProject,
  toDependency,
  parseAnalysisStatus,
  VsiFilePath (..), -- data constructor only exported for testing
  VsiRulePath (..), -- data constructor only exported for testing
  VsiInference (..),
  VsiRule (..),
  VsiExportedInferencesBody (..),
  generateRules,
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), ToJSONKey, defaultOptions, genericToEncoding, withObject, (.!=), (.:), (.:?))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString)
import Data.String.Conversion (ToString, ToText, toText)
import Data.Text (Text, isPrefixOf)
import Data.Text qualified as Text
import DepTypes (DepType (..), Dependency (..), VerConstraint (CEq))
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
import Effect.Logger (Pretty (pretty), viaShow)
import GHC.Generics (Generic)
import Srclib.Converter (depTypeToFetcher, fetcherToDepType)
import Srclib.Types qualified as Srclib

-- | The VSI backend returns a scan ID when a scan is created, which is then used to add files to the scan and get inferred OSS dependencies.
newtype ScanID = ScanID {unScanID :: Text} deriving (ToJSON, FromJSON, Eq, Ord, IsString)

instance Show ScanID where
  show (ScanID scanId) = show scanId

-- | The VSI backend returns statuses for tracking which stage analysis is on.
-- Programmatically we only care about some of these, the rest are informational and can be safely shown to a user to indicate activity.
data AnalysisStatus
  = AnalysisPending
  | AnalysisFinished
  | AnalysisFailed
  | AnalysisInformational Text

parseAnalysisStatus :: Text -> AnalysisStatus
parseAnalysisStatus status = case status of
  "NOT_STARTED" -> AnalysisPending
  "DONE" -> AnalysisFinished
  "FAILED" -> AnalysisFailed
  other -> AnalysisInformational other

-- | VSI supports a subset of possible Locators.
-- Specifically, all VSI locators must have a valid revision.
data Locator = Locator
  { locatorFetcher :: Text
  , locatorProject :: Text
  , locatorRevision :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Locator where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Locator where
  parseJSON = withObject "Locator" $ \obj -> do
    Locator
      <$> obj .: "fetcher"
      <*> obj .: "package"
      <*> obj .: "revision"

parseLocator :: (ToText a) => a -> Either LocatorParseError Locator
parseLocator = validateLocator . Srclib.parseLocator . toText

renderLocator :: Locator -> Text
renderLocator Locator{..} = locatorFetcher <> "+" <> locatorProject <> "$" <> locatorRevision

newtype LocatorParseError = RevisionRequired Srclib.Locator
  deriving (Eq, Ord, Show)

instance ToText LocatorParseError where
  toText (RevisionRequired locator) =
    "Revision is required on locator: " <> Srclib.renderLocator locator

instance ToDiagnostic LocatorParseError where
  renderDiagnostic (RevisionRequired locator) =
    "Revision is required on locator: " <> viaShow locator

-- | VSI locally resolves the dependencies of some VSI dependencies using the FOSSA API.
-- In the case where a user doesn't have access to view a project that is a dependency of their project,
-- we can't perform this resolution.
-- To handle this case we provide users with an escape hatch, which is an argument allowing them to skip resolving some of their dependencies.
-- This type canonicalizes that request.
newtype SkipResolution = SkipResolution {unVSISkipResolution :: Set Locator}
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SkipResolution where
  toEncoding = genericToEncoding defaultOptions

shouldSkipResolving :: SkipResolution -> Locator -> Bool
shouldSkipResolving skip loc = loc `Set.member` (unVSISkipResolution skip)

toDependency :: Locator -> Either ToDependencyError Dependency
toDependency locator =
  Dependency
    <$> validateDepType locator
    <*> Right (locatorProject locator)
    <*> (Right . Just . CEq $ locatorRevision locator)
    <*> Right []
    <*> Right mempty
    <*> Right mempty

validateDepType :: Locator -> Either ToDependencyError DepType
validateDepType locator = case fetcherToDepType (locatorFetcher locator) of
  Nothing -> Left $ UnsupportedLocator locator
  Just dt -> Right dt

newtype ToDependencyError = UnsupportedLocator Locator
  deriving (Eq, Ord, Show)

instance ToDiagnostic ToDependencyError where
  renderDiagnostic (UnsupportedLocator locator) =
    "Unsupported locator: Cannot convert fetcher "
      <> pretty (locatorFetcher locator)
      <> " to known dependency type. Locator: "
      <> viaShow locator

validateLocator :: Srclib.Locator -> Either LocatorParseError Locator
validateLocator loc = Locator (Srclib.locatorFetcher loc) (Srclib.locatorProject loc) <$> validateRevision loc

validateRevision :: Srclib.Locator -> Either LocatorParseError Text
validateRevision loc = case (Srclib.locatorRevision loc) of
  Nothing -> Left $ RevisionRequired loc
  Just r -> Right r

isUserDefined :: Locator -> Bool
isUserDefined loc = locatorFetcher loc == userDefinedFetcher

userDefinedFetcher :: Text
userDefinedFetcher = "iat"

isTopLevelProject :: Locator -> Bool
isTopLevelProject loc = locatorFetcher loc == depTypeToFetcher CustomType

-- |A path returned on inferences from sherlock-api.
--
-- While 'VsiFilePath's look like filepaths, we treat them as text rather than 'FilePath' or 'Path's.
-- Those packages behave based on on the platform they're compiled on, while VSI always uses unix-style paths.
newtype VsiFilePath = VsiFilePath {unVsiFilePath :: Text}
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)

-- |A path for a VSI rule.
-- During processing we change a list of file paths to directory paths for inclusion in rules.
newtype VsiRulePath = VsiRulePath {unVsiRulePath :: Text}
  deriving newtype (Eq, Ord, Show, ToJSON, ToJSONKey, ToString)

newtype VsiInference = VsiInference
  {inferenceLocator :: Maybe Locator}
  deriving (Eq, Ord, Show)

instance FromJSON VsiInference where
  parseJSON = withObject "VsiInference" $ \obj -> do
    loc <- obj .: "Locator"
    -- An empty string locator is Nothing, but a missing or malformed one is not ok.
    case loc :: Text of
      "" -> pure $ VsiInference Nothing
      s -> either (fail . show) (pure . VsiInference . Just) (parseLocator s)

newtype VsiExportedInferencesBody = VsiExportedInferencesBody
  { unVsiExportedInferencesBody :: Map.Map VsiFilePath VsiInference
  }
  deriving (Eq, Ord, Show)

instance FromJSON VsiExportedInferencesBody where
  parseJSON = withObject "VsiExportedInferencesBody" $ \obj -> do
    inferences <- (obj .:? "InferencesByFilePath") .!= KeyMap.empty
    parsedVals <- traverse parseJSON inferences
    pure . VsiExportedInferencesBody . Map.mapKeys VsiFilePath . KeyMap.toMapText $ parsedVals

data VsiRule = VsiRule
  { vsiRulePath :: VsiRulePath
  , vsiRuleLocator :: Locator
  }
  deriving (Eq, Ord, Show)

instance ToJSON VsiRule where
  toJSON (VsiRule rulePath ruleLocator) = toJSON $ Map.singleton rulePath (renderLocator ruleLocator)

-- |  Match each inference locator to the shortest filepath directory prefix for that locator.
generateRules :: VsiExportedInferencesBody -> [VsiRule]
generateRules inferenceBody = do
  (loc, paths) <- locatorPaths
  path <- NE.toList paths
  [VsiRule path loc]
  where
    locatorPaths :: [(Locator, NE.NonEmpty VsiRulePath)]
    locatorPaths =
      Map.toList
        . (fmap getPrefixes)
        . Map.foldrWithKey makeLocatorPathLists Map.empty
        . unVsiExportedInferencesBody
        $ inferenceBody

    -- Turn locators into rule paths and skipping empty paths.
    makeLocatorPathLists :: VsiFilePath -> VsiInference -> Map.Map Locator (NE.NonEmpty VsiFilePath) -> Map.Map Locator (NE.NonEmpty VsiFilePath)
    makeLocatorPathLists filePath VsiInference{inferenceLocator} m =
      case inferenceLocator of
        Nothing -> m
        Just inferenceLocator' -> Map.insertWith (<>) inferenceLocator' (NE.singleton filePath) m

-- |Get the shortest prefixes for every filepath in a list.
-- This works by first sorting the list lexicographically and then storing only the initial path that prefixes each group of filepaths.
--
-- Ex: ["/foo/bar/baz.c", "/foo/hello.c". "/other/dir/world.c"] -> ["/foo", "/other/dir"]
getPrefixes :: NE.NonEmpty VsiFilePath -> NE.NonEmpty VsiRulePath
getPrefixes paths = snd . foldr accumPrefixes startVal $ (NE.tail sorted)
  where
    startVal :: (VsiRulePath, NE.NonEmpty VsiRulePath)
    startVal = (sortedHead, NE.singleton sortedHead)

    sorted :: NE.NonEmpty VsiFilePath
    sorted = NE.sort paths

    sortedHead :: VsiRulePath
    sortedHead = parentDir . NE.head $ sorted

    accumPrefixes :: VsiFilePath -> (VsiRulePath, NE.NonEmpty VsiRulePath) -> (VsiRulePath, NE.NonEmpty VsiRulePath)
    accumPrefixes filePath (currPrefix, acc) =
      if currPrefix `prefixesFilePath` filePath
        then (currPrefix, acc)
        else
          let parent = parentDir filePath
           in (parent, parent <| acc)

    parentDir :: VsiFilePath -> VsiRulePath
    parentDir = VsiRulePath . Text.dropEnd 1 . Text.dropWhileEnd (/= '/') . unVsiFilePath

    prefixesFilePath :: VsiRulePath -> VsiFilePath -> Bool
    prefixesFilePath rp fp = unVsiRulePath rp `isPrefixOf` unVsiFilePath fp
