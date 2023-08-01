module App.Fossa.Grep (
  analyzeWithGrep,
) where

import Control.Carrier.Diagnostics (Diagnostics)
import Control.Effect.Debug (Debug)
import Control.Effect.Lift (Has, Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Data.List.NonEmpty (NonEmpty)

import App.Fossa.Config.Analyze (GrepEntry (grepEntryMatchCriteria, grepEntryName), GrepOptions (..))
import Data.Aeson (ToJSON (toEncoding, toJSON), defaultOptions, encode, genericToEncoding)
import Data.Functor.Extra ((<$$>))
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (pretty)
import Effect.Exec (Exec)
import Effect.Logger (Logger, logInfo)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)
import Srclib.Types (SourceUnit (..))

data LernieConfig = LernieConfig
  { rootDir :: Path Abs Dir
  , regexes :: NonEmpty LernieRegex
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON LernieConfig where
  toEncoding = genericToEncoding defaultOptions

data LernieRegex = LernieRegex
  { pattern :: Text
  , name :: Text
  , scanType :: GrepScanType
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON LernieRegex where
  toEncoding = genericToEncoding defaultOptions

data GrepScanType = CustomLicense | KeywordSearch
  deriving (Eq, Ord, Show, Generic)

instance ToText GrepScanType where
  toText CustomLicense = "CustomLicense"
  toText KeywordSearch = "KeywordSearch"

instance ToJSON GrepScanType where
  toJSON = toJSON . toText

analyzeWithGrep ::
  ( Has Diagnostics sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has Debug sig m
  , Has Exec sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  GrepOptions ->
  m (Maybe SourceUnit) -- TODO: make the types that we actually want to return
analyzeWithGrep rootDir maybeApiOpts grepOptions = do
  -- TODO: convert grepOptions to lernieOpts
  let maybeLernieConfig = grepOptionsToLernieConfig rootDir grepOptions
  let t = regexes <$> maybeLernieConfig
  logInfo . pretty $ "lernie config: " <> show maybeLernieConfig
  pure Nothing

grepOptionsToLernieConfig :: Path Abs Dir -> GrepOptions -> Maybe LernieConfig
grepOptionsToLernieConfig rootDir grepOptions =
  case regexes of
    Nothing -> Nothing
    Just res -> Just $ LernieConfig rootDir res
  where
    customLicenseSearches = grepEntryToLernieRegex CustomLicense <$$> customLicenseSearch grepOptions
    keywordSearches = grepEntryToLernieRegex KeywordSearch <$$> keywordSearch grepOptions

    regexes = case (customLicenseSearches, keywordSearches) of
      (Nothing, Just grepEntries) -> Just grepEntries
      (Just grepEntries, Nothing) -> Just grepEntries
      (Just customLicenseEntries, Just keywordEntries) -> Just $ customLicenseEntries <> keywordEntries
      (Nothing, Nothing) -> Nothing

grepEntryToLernieRegex :: GrepScanType -> GrepEntry -> LernieRegex
grepEntryToLernieRegex scanType grepEntry =
  LernieRegex (grepEntryMatchCriteria grepEntry) (grepEntryName grepEntry) scanType
