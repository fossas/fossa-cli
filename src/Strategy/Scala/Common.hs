module Strategy.Scala.Common (
  removeLogPrefixes,
  SbtArtifact (..),
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

data SbtArtifact = SbtArtifact
  { groupId :: Text
  , artifactId :: Text
  , version :: Text
  }
  deriving (Eq, Ord, Show)

-- | Removes log prefix from the log.
-- >> removeLogPrefixes "[info] someInfo" = "someInfo"
removeLogPrefixes :: Text -> Text
removeLogPrefixes content = Text.unlines . map removePrefix $ Text.lines content
  where
    removePrefix candidate
      | Text.isPrefixOf "[debug]" candidate = fromMaybe candidate $ Text.stripPrefix "[debug] " candidate
      | Text.isPrefixOf "[info]" candidate = fromMaybe candidate $ Text.stripPrefix "[info] " candidate
      | Text.isPrefixOf "[warn]" candidate = fromMaybe candidate $ Text.stripPrefix "[warn] " candidate
      | Text.isPrefixOf "[success]" candidate = fromMaybe candidate $ Text.stripPrefix "[success] " candidate
      | Text.isPrefixOf "[error]" candidate = fromMaybe candidate $ Text.stripPrefix "[error] " candidate
      | Text.isPrefixOf "[trace]" candidate = fromMaybe candidate $ Text.stripPrefix "[trace] " candidate
      | otherwise = candidate
