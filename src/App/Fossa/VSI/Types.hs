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
  uniqueVsiLocators,
  VsiFilePath (..), -- data constructor only exported for testing
  VsiInference (..),
  VsiExportedInferencesBody (..),
) where

import Control.Effect.Diagnostics (ToDiagnostic, renderDiagnostic)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), defaultOptions, genericToEncoding, withObject, (.!=), (.:), (.:?))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString)
import Data.String.Conversion (ToText, toText)
import Data.Text (Text)
import DepTypes (DepType (..), Dependency (..), VerConstraint (CEq))
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

newtype VsiFilePath = VsiFilePath Text
  deriving newtype (Eq, Ord, Show, FromJSON)

-- |Locator output of /inferences.
-- The output of the /inferences endpoint will sometimes return a locator of "".
-- Dealing with this is left to the caller which is why it is only a wrapper of Text.
newtype VsiLocator = VsiLocator Text
  deriving newtype (Eq, Ord, Show, FromJSON, ToText, IsString)

-- There are other fields on the returned data, but we don't use them.
newtype VsiInference = VsiInference
  { -- TODO: Is it let confusing to just make this a "Maybe"?
    -- Unparsed text because the endpoint can return "".
    -- Dealing with this is left to the caller.
    inferenceLocator :: VsiLocator
  }
  deriving (Eq, Ord, Show)

instance FromJSON VsiInference where
  parseJSON = withObject "VsiInference" $ \obj -> do
    VsiInference <$> obj .: "Locator"

newtype VsiExportedInferencesBody = VsiExportedInferencesBody
  { unVsiExportedInferencesBody :: Map.Map VsiFilePath VsiInference
  }
  deriving (Eq, Ord, Show)

uniqueVsiLocators :: VsiExportedInferencesBody -> [VsiLocator]
uniqueVsiLocators =
  Set.toList
    . Map.foldl' addLocators Set.empty
    . unVsiExportedInferencesBody
  where
    addLocators :: Set.Set VsiLocator -> VsiInference -> Set.Set VsiLocator
    addLocators s VsiInference{inferenceLocator} = Set.insert inferenceLocator s

instance FromJSON VsiExportedInferencesBody where
  parseJSON = withObject "VsiExportedInferencesBody" $ \obj -> do
    inferences <- (obj .:? "InferencesByFilePath") .!= KeyMap.empty
    parsedVals <- traverse parseJSON inferences :: Parser (KeyMap.KeyMap VsiInference)
    pure . VsiExportedInferencesBody . Map.mapKeys VsiFilePath . KeyMap.toMapText $ parsedVals
