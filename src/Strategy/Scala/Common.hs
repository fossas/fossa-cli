module Strategy.Scala.Common (
  removeLogPrefixes,
  withoutStdLibs,
  SbtArtifact (..),
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (DepType (MavenType), Dependency (dependencyName, dependencyType))
import Graphing (Graphing, filter)

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

-- | Removes scala standard library from sbt
-- Refer: https://github.com/sbt/sbt/blob/master/main/src/main/scala/sbt/internal/graph/GraphTransformations.scala#L39
--
-- Note that, as general case FOSSA CLI prefers not to report standard library,
-- to avoid dependency noise, issue noise arising from inclusion of standard library.
withoutStdLibs :: Graphing Dependency -> Graphing Dependency
withoutStdLibs = Graphing.filter (not . isScalaStdLib)
  where
    isScalaStdLib :: Dependency -> Bool
    isScalaStdLib d =
      dependencyType d == MavenType
        && ( (dependencyName d == "org.scala-lang:scala3-library_3") -- https://mvnrepository.com/artifact/org.scala-lang/scala3-library_3/
              || (dependencyName d == "org.scala-lang:scala3-library") -- https://mvnrepository.com/artifact/org.scala-lang/scala3-library/
              || (dependencyName d == "org.scala-lang:scala-library") -- https://mvnrepository.com/artifact/org.scala-lang/scala-library/ (moved to scala3-library now, included for legacy)
           )
