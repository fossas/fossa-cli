{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Analyze.Record (
  AnalyzeJournal (..),
  AnalyzeEffects (..),
  saveReplayLog,
  loadReplayLog,
) where

import App.Version (fullVersionDescription)
import Control.Effect.Record
import Data.Aeson
import Data.Text (Text)
import Effect.Exec (ExecF)
import Effect.ReadFS (ReadFSF)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)

data AnalyzeJournal = AnalyzeJournal
  { analyzeCommit :: Text
  , analyzeEffects :: AnalyzeEffects
  , analyzeArgs :: [String]
  , analyzeWorkdir :: FilePath
  }
  deriving (Eq, Ord, Show)

data AnalyzeEffects = AnalyzeEffects
  { effectsReadFS :: Journal ReadFSF
  , effectsExec :: Journal ExecF
  }
  deriving (Eq, Ord, Show)

instance FromJSON AnalyzeJournal where
  parseJSON = withObject "AnalyzeJournal" $ \obj ->
    AnalyzeJournal
      <$> obj .: "commit"
      <*> obj .: "effects"
      <*> obj .: "args"
      <*> obj .: "workdir"

instance FromJSON AnalyzeEffects where
  parseJSON = withObject "AnalyzeEffects" $ \obj ->
    AnalyzeEffects
      <$> obj .: "ReadFS"
      <*> obj .: "Exec"

instance ToJSON AnalyzeJournal where
  toJSON AnalyzeJournal{..} =
    object
      [ "commit" .= analyzeCommit
      , "args" .= analyzeArgs
      , "workdir" .= analyzeWorkdir
      , "effects" .= analyzeEffects
      ]

instance ToJSON AnalyzeEffects where
  toJSON AnalyzeEffects{..} =
    object
      [ "ReadFS" .= effectsReadFS
      , "Exec" .= effectsExec
      ]

saveReplayLog :: Journal ReadFSF -> Journal ExecF -> FilePath -> IO ()
saveReplayLog readFSJournal execJournal path = do
  args <- getArgs
  workdir <- getCurrentDirectory
  let effects =
        AnalyzeEffects
          { effectsReadFS = readFSJournal
          , effectsExec = execJournal
          }

      journal =
        AnalyzeJournal
          { analyzeCommit = fullVersionDescription
          , analyzeEffects = effects
          , analyzeArgs = args
          , analyzeWorkdir = workdir
          }

  encodeFile path journal

loadReplayLog :: FilePath -> IO (Either String AnalyzeJournal)
loadReplayLog = eitherDecodeFileStrict'
