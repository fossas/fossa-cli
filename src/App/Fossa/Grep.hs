{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Grep (
  analyzeWithGrep,
) where

import Control.Carrier.Diagnostics (Diagnostics, context)
import Control.Effect.Debug (Debug)
import Control.Effect.Lift (Has, Lift)
import Control.Effect.StickyLogger (StickyLogger)
import Data.List.NonEmpty (NonEmpty)

import App.Fossa.Config.Analyze (GrepEntry (grepEntryMatchCriteria, grepEntryName), GrepOptions (..))
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withLernieBinary)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), encode, object)
import Data.Aeson qualified as Aeson
import Data.Functor.Extra ((<$$>))
import Data.String.Conversion (ToText (toText), decodeUtf8)
import Data.Text (Text)
import Data.Void (Void)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execParser')
import Effect.Logger (Logger, logInfo)
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)
import Prettyprinter (Pretty (pretty))
import Text.Megaparsec (Parsec)

data LernieConfig = LernieConfig
  { rootDir :: Path Abs Dir
  , regexes :: NonEmpty LernieRegex
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON LernieConfig where
  toJSON LernieConfig{..} =
    object
      [ "root_dir" .= toText rootDir
      , "regexes" .= toJSON regexes
      ]

data LernieRegex = LernieRegex
  { pattern :: Text
  , name :: Text
  , scanType :: GrepScanType
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON LernieRegex where
  toJSON LernieRegex{..} =
    object
      [ "pattern" .= toText pattern
      , "name" .= toText name
      , "scan_type" .= toText scanType
      ]

data GrepScanType = CustomLicense | KeywordSearch
  deriving (Eq, Ord, Show, Generic)

instance ToText GrepScanType where
  toText CustomLicense = "CustomLicense"
  toText KeywordSearch = "KeywordSearch"

instance ToJSON GrepScanType where
  toJSON = toJSON . toText

analyzeWithGrep ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  GrepOptions ->
  m (Maybe Text) -- TODO: make the types that we actually want to return
analyzeWithGrep rootDir maybeApiOpts grepOptions = do
  -- TODO: convert grepOptions to lernieOpts
  let maybeLernieConfig = grepOptionsToLernieConfig rootDir grepOptions
  logInfo . pretty $ "lernie config: " <> show maybeLernieConfig
  logInfo . pretty $ show $ encode maybeLernieConfig
  case maybeLernieConfig of
    Just (lernieConfig) -> Just <$> runLernie lernieConfig
    Nothing -> pure Nothing

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

runLernie ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  LernieConfig ->
  m Text
runLernie lernieConfig = withLernieBinary $ \bin -> do
  -- Handle the JSON response.
  let lernieConfigJSON = decodeUtf8 $ Aeson.encode lernieConfig
  execParser' parseLernieOutput (lernieCommand bin) lernieConfigJSON

type Parser = Parsec Void Text

lernieCommand :: BinaryPaths -> Command
lernieCommand bin =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = ["--config", "-"]
    , cmdAllowErr = Never
    }

parseLernieOutput :: Parser Text
parseLernieOutput = "type"
