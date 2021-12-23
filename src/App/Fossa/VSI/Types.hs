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
) where

import Control.Effect.Diagnostics (ToDiagnostic, renderDiagnostic)
import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (ToText, toText)
import Data.Text (Text)
import DepTypes (DepType (..), Dependency (..), VerConstraint (CEq))
import Effect.Logger (Pretty (pretty), viaShow)
import Srclib.Converter (depTypeToFetcher, fetcherToDepType)
import Srclib.Types qualified as Srclib

-- | The VSI backend returns a scan ID when a scan is created, which is then used to add files to the scan and get inferred OSS dependencies.
newtype ScanID = ScanID {unScanID :: Text} deriving (ToJSON, FromJSON)

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
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

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
