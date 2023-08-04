{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use join" #-}

module App.Fossa.Grep (
  analyzeWithGrep,
) where

import App.Fossa.Config.Analyze (GrepEntry (grepEntryMatchCriteria, grepEntryName), GrepOptions (..))
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withLernieBinary)
import Control.Carrier.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON (toJSON), Value (Object), decode, eitherDecode, encode, object, withObject, withText)
import Data.Aeson qualified as A
import Data.Aeson qualified as Aeson
import Data.Aeson.Types ((.:))
import Data.ByteString.Lazy qualified as BL
import Data.Functor.Extra ((<$$>))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (mapMaybe)
import Data.String.Conversion (ToText (toText), decodeUtf8)
import Data.Text (Text)
import Debug.Trace (traceM)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execThrow'')
import Effect.Logger (Logger, logInfo)
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)
import Prettyprinter (Pretty (pretty))

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

instance FromJSON GrepScanType

data LernieMessageType = LernieMessageTypeMatch | LernieMessageTypeError | LernieMessageTypeWarning
  deriving (Eq, Ord, Show, Generic)

instance ToText LernieMessageType where
  toText LernieMessageTypeMatch = "Match"
  toText LernieMessageTypeError = "Error"
  toText LernieMessageTypeWarning = "Warning"

instance ToJSON LernieMessageType where
  toJSON = toJSON . toText

instance FromJSON LernieMessageType where
  parseJSON = withText "LernieMessageType" $ \msg -> do
    case msg of
      "Match" -> pure LernieMessageTypeMatch
      "Error" -> pure LernieMessageTypeError
      _ -> pure LernieMessageTypeWarning

data LernieMatch = LernieMatch
  { lernieMatchPath :: Text
  , lernieMatchMatches :: [LernieMatchData]
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LernieMatch where
  parseJSON = withObject "LernieMatch" $ \obj ->
    LernieMatch
      <$> (obj .: "path")
      <*> (obj .: "matches")

data LernieWarning = LernieWarning
  { lernieWarningMessage :: Text
  , lernieWarningType :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LernieWarning where
  parseJSON = withObject "LernieWarning" $ \obj ->
    LernieWarning
      <$> (obj .: "message")
      <*> (obj .: "type")

data LernieError = LernieError
  { lernieErrorMessage :: Text
  , lernieErrorType :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LernieError where
  parseJSON = withObject "LernieError" $ \obj ->
    LernieError
      <$> (obj .: "message")
      <*> (obj .: "type")

data LernieMatchData = LernieMatchData
  { lernieMatchDataPattern :: Text
  , lernieMatchDataMatchString :: Text
  , lernieMatchDataScanType :: GrepScanType
  , lernieMatchDataName :: Text
  , lernieMatchDataStartByte :: Int
  , lernieMatchDataEndByte :: Int
  , lernieMatchDataStartLine :: Int
  , lernieMatchDataEndLine :: Int
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LernieMatchData where
  parseJSON = withObject "LernieMatchData" $ \obj ->
    LernieMatchData
      <$> (obj .: "pattern")
      <*> (obj .: "match_str")
      <*> (obj .: "scan_type")
      <*> (obj .: "name")
      <*> (obj .: "start_byte")
      <*> (obj .: "end_byte")
      <*> (obj .: "start_line")
      <*> (obj .: "end_line")

instance ToJSON LernieMatchData

data LernieMessages = LernieMessages
  { lernieMessageWarnings :: [LernieWarning]
  , lernieMessageErrors :: [LernieError]
  , lernieMessageMatches :: [LernieMatch]
  }
  deriving (Eq, Ord, Show, Generic)

data LernieMessage = LernieMessageLernieMatch LernieMatch | LernieMessageLernieWarning LernieWarning | LernieMessageLernieError LernieError
  deriving (Eq, Ord, Show, Generic)

emptyLernieMessages :: LernieMessages
emptyLernieMessages = LernieMessages [] [] []

instance FromJSON LernieMessage where
  parseJSON (Object o) = do
    messageType <- o .: "type"
    traceM $ "message type = " ++ show messageType
    case messageType of
      LernieMessageTypeWarning -> do
        Object d <- o .: "data"
        let message = d .: "message"
        let message_type = d .: "type"
        warning <- LernieWarning <$> message_type <*> message
        pure $ LernieMessageLernieWarning warning
      LernieMessageTypeError -> do
        Object d <- o .: "data"
        let message_type = d .: "type"
        let message = d .: "message"
        err <- LernieError <$> message_type <*> message
        pure $ LernieMessageLernieError err
      LernieMessageTypeMatch -> do
        Object d <- o .: "data"
        let path = d .: "path"
        let matches = d .: "matches"
        match <- LernieMatch <$> path <*> matches
        pure $ LernieMessageLernieMatch match
  parseJSON _ = fail "Invalid schema for LernieMessage. It must be an object"

addLernieMessage :: LernieMessage -> LernieMessages -> LernieMessages
addLernieMessage message existing = case message of
  LernieMessageLernieMatch msg -> existing{lernieMessageMatches = msg : lernieMessageMatches existing}
  LernieMessageLernieWarning msg -> existing{lernieMessageWarnings = msg : lernieMessageWarnings existing}
  LernieMessageLernieError msg -> existing{lernieMessageErrors = msg : lernieMessageErrors existing}

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
  m (Maybe LernieMessages)
analyzeWithGrep rootDir _maybeApiOpts grepOptions = do
  -- TODO: convert grepOptions to lernieOpts
  let maybeLernieConfig = grepOptionsToLernieConfig rootDir grepOptions
  logInfo . pretty $ "lernie config: " <> show maybeLernieConfig
  logInfo . pretty $ show $ encode maybeLernieConfig
  case maybeLernieConfig of
    Just (lernieConfig) -> do
      messages <- runLernie lernieConfig
      pure $ Just messages
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
  , Has Logger sig m
  ) =>
  LernieConfig ->
  m LernieMessages
runLernie lernieConfig = withLernieBinary $ \bin -> do
  -- Handle the JSON response.
  let lernieConfigJSON = decodeUtf8 $ Aeson.encode lernieConfig
  -- _ <- pure $ runIt $ lernieCommand bin
  result <- execThrow'' (lernieCommand bin) lernieConfigJSON
  logInfo . pretty $ "Lernie result: " <> show result
  let messageLines = BL.splitWith (== 10) result
      inds :: [Either String LernieMessage]
      inds = map eitherDecode messageLines
  logInfo . pretty $ "parsing this many lines: " ++ show messageLines
  logInfo . pretty $ "Lernie messages " <> show inds
  let messages = parseLernieJson result
  logInfo . pretty $ "Lernie messages " <> show messages
  pure messages

parseLernieJson :: BL.ByteString -> LernieMessages
parseLernieJson out =
  foldr addLernieMessage emptyLernieMessages parsedLines
  where
    messageLines = BL.splitWith (== 10) out
    parsedLines :: [LernieMessage]
    parsedLines = mapMaybe decode messageLines

lernieCommand :: BinaryPaths -> Command
lernieCommand bin =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = ["--config", "-"]
    , cmdAllowErr = Never
    }

-- runIt :: (Has (Lift IO) sig m) => Command -> m ()
-- runIt cmd = do
--   let path = toString $ cmdName cmd
--   let args = " --config /Users/scott/tmp/lernie.json"
--   (CP.Inherited, fromProcess, CP.ClosedStream, cph) <- sendIO $ CP.streamingProcess (CP.shell $ path ++ args)
--   -- res <- runConduit $ yieldMany [1 .. 10] .| iterMC print .| sumC
--   -- print res
--   -- pure "foo"
--   let output = runConduit $ fromProcess .| CL.mapM_ (\bs -> putStrLn $ "from process: " ++ show bs)
--   -- o <- runConduit $ fromProcess .| (CB.sourceHandle stdout) .| maybeValueParser
--   -- let output = runConduit $ stdout .| CL.mapM_ (\bs -> putStrLn $ "from process: " ++ show bs)
--   ec <- sendIO . runConcurrently $ Concurrently output *> Concurrently (CP.waitForStreamingProcess cph)
--   -- ec <- out
--   pure ()

-- -- ec <- runConcurrently $ Concurrently output *> Concurrently (waitForStreamingProcess cph)
-- putStrLn $ "Process exit code: " ++ show ec
-- pure ec
