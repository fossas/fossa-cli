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
    removePrefix :: Text -> Text
    removePrefix candidate
      | Text.isPrefixOf "[debug]" candidate = maybeRemovePrefix "[debug]" candidate
      | Text.isPrefixOf "[info]" candidate = maybeRemovePrefix "[info]" candidate
      | Text.isPrefixOf "[warn]" candidate = maybeRemovePrefix "[warn]" candidate
      | Text.isPrefixOf "[success]" candidate = maybeRemovePrefix "[success]" candidate
      | Text.isPrefixOf "[error]" candidate = maybeRemovePrefix "[error]" candidate
      | Text.isPrefixOf "[trace]" candidate = maybeRemovePrefix "[trace]" candidate
      | otherwise = candidate

    maybeRemovePrefix :: Text -> Text -> Text
    maybeRemovePrefix prefix candidate = fromMaybe candidate $ Text.stripPrefix (prefix <> " ") candidate
