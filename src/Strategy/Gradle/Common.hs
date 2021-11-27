module Strategy.Gradle.Common (
  packagePathsWithJson,
  getLinesWithPrefix,
  configNameToLabel,
  getWarningMessages,
  ConfigName (..),
  GradleLabel (..),
  PackageName (..),
) where

import Data.Aeson (FromJSON)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (DepEnvironment (..))
import Strategy.Android.Util (isDefaultAndroidDevConfig, isDefaultAndroidTestConfig)

newtype ConfigName = ConfigName {unConfigName :: Text} deriving (Eq, Ord, Show, FromJSON)
newtype GradleLabel = Env DepEnvironment deriving (Eq, Ord, Show)
newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Ord, Show, FromJSON)

packagePathsWithJson :: [Text] -> [(PackageName, Text)]
packagePathsWithJson = map (\line -> let (x, y) = Text.breakOn "_{" line in (PackageName x, Text.drop 1 y))

getLinesWithPrefix :: Text -> Text -> [Text]
getLinesWithPrefix text prefix = prefixLines
  where
    textLines :: [Text]
    textLines = Text.lines (Text.filter (/= '\r') text)

    prefixLines :: [Text]
    prefixLines = mapMaybe (Text.stripPrefix prefix) textLines

configNameToLabel :: Text -> GradleLabel
configNameToLabel conf =
  case conf of
    "compileOnly" -> Env EnvDevelopment
    x | x `elem` ["testImplementation", "testCompileOnly", "testRuntimeOnly", "testCompileClasspath", "testRuntimeClasspath"] -> Env EnvTesting
    x | isDefaultAndroidDevConfig x -> Env EnvDevelopment
    x | isDefaultAndroidTestConfig x -> Env EnvTesting
    x -> Env $ EnvOther x

-- | Gets the warning messages from jsondeps gradle script stdout text
--
-- >>> getWarningMessages "FOSSA-WARNING (some scope): some message/n DEBUG (some scope): some debug message"
-- ["some message"]
getWarningMessages :: Text -> [Text]
getWarningMessages text = map (Text.strip . Text.drop 2 . snd . Text.breakOn ") ") $ getLinesWithPrefix text "FOSSA-WARNING"