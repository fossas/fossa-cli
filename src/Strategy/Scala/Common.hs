module Strategy.Scala.Common (
  removeLogPrefixes,
  SbtArtifact (..),
  mkSbtCommand,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Effect.Exec (AllowErr (Never), Command (..))

-- | Generate an sbt sub-command using which turns off colored output and ensures it does not enter repl mode.
--
--  example:
--  mkSbtCommand "dependencyTree"  results in "sbt -batch -no-colors dependencyTree" as a CLI command.
mkSbtCommand :: Text -> Command
mkSbtCommand cmdName =
  Command
    { cmdName = "sbt"
    , -- Use single hyphens rather than double-hyphens for old sbt compatibility.
      -- Ex: -batch instead of --batch
      cmdArgs =
        [ "-batch"
        , "-no-colors"
        , cmdName
        ]
    , cmdAllowErr = Never
    }

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
