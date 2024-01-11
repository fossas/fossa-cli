module Strategy.Gradle.Common (
  packagePathsWithJson,
  getLinesWithPrefix,
  configNameToLabel,
  getDebugMessages,
  ConfigName (..),
  GradleLabel (..),
  PackageName (..),
  ProjectName (..),

  -- * for testing
  getLogWithPrefix,
) where

import Data.Aeson (FromJSON)
import Data.Maybe (mapMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (DepEnvironment (..))
import Strategy.Android.Util (isDefaultAndroidDevConfig, isDefaultAndroidTestConfig)

newtype ConfigName = ConfigName {unConfigName :: Text} deriving (Eq, Ord, Show, FromJSON, IsString)
newtype GradleLabel = Env DepEnvironment deriving (Eq, Ord, Show)
newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Ord, Show, FromJSON, IsString)
newtype ProjectName = ProjectName {unProjectName :: Text} deriving (Eq, Ord, Show, FromJSON, IsString)

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
    x | x `elem` knownTestConfigs -> Env EnvTesting
    x | isDefaultAndroidDevConfig x -> Env EnvDevelopment
    x | isDefaultAndroidTestConfig x -> Env EnvTesting
    x -> Env $ EnvOther x

knownTestConfigs :: [Text]
knownTestConfigs =
  [ "testImplementation"
  , "testCompileOnly"
  , "testRuntimeOnly"
  , "testCompileClasspath"
  , "testRuntimeClasspath"
  , "testFixturesApi"
  , "testFixturesImplementation"
  , "testFixturesRuntimeOnly"
  , "testFixturesCompileOnly"
  , "testFixturesRuntimeClasspath"
  , "testFixturesCompileClasspath"
  , "testFixturesApiElements"
  ]

getDebugMessages :: Text -> [Text]
getDebugMessages text = getLogWithPrefix text "FOSSA-DEBUG"

-- | Gets the log message for matching identifier. Applies to gradle init script log format.
-- Log format of the gradle script is: 'identifier (scope): message'
--
-- >>> getLogWithPrefix "SOME-PREFIX (some scope): some message/n DEBUG (some scope): some debug message" "SOME-PREFIX"
-- ["some message"]
getLogWithPrefix :: Text -> Text -> [Text]
getLogWithPrefix text prefix = map (Text.strip . Text.drop 2 . snd . Text.breakOn "):") $ getLinesWithPrefix text prefix
