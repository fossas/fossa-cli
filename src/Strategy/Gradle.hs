
{-# language QuasiQuotes #-}

module Strategy.Gradle
  (
  ) where

import Prologue hiding (many)

import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Char (isLetter)
import qualified Data.Text as T
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Diagnostics
import           Discovery.Walk
import           Effect.Exec
import           Effect.ReadFS
import qualified Graph as G
import           Types

import Path.IO (WalkAction)

data GradleOpts = GradleOpts
  { gradleOptsDir      :: Path Rel Dir
  , gradleOptsProjects :: [Text]
  --, gradleOptsCmd               :: Text
  --, gradleOptsTask              :: Text
  --, gradleOptsOnline            :: Bool
  --, gradleOptsAllSubmodules     :: Bool
  --, gradleOptsAllConfigurations :: Bool
  --, gradleOptsTimeout           :: Text -- TODO: Duration
  --, gradleOptsRetries           :: Int
  --, gradleOptsProject           :: Text
  --, gradleOptsConfiguration     :: Text
  } deriving (Show, Generic)

instance FromJSON GradleOpts where
  parseJSON = withObject "GradleOpts" $ \obj ->
    GradleOpts <$> obj .: "dir"
               <*> obj .: "project"

instance ToJSON GradleOpts where
  toJSON GradleOpts{..} = object ["dir" .= gradleOptsDir, "projects" .= gradleOptsProjects]

gradleTasksCmd :: Command
gradleTasksCmd = Command
  { cmdNames = [[relfile|gradlew|], [relfile|gradlew.bat|], [relfile|gradle|]]
  , cmdBaseArgs = ["tasks", "--all"]
  , cmdAllowErr = Never
  }

type Parser = Parsec Void Text

data GradleTasksOutput

gradleDepTasksParser :: Parser [Text]
gradleDepTasksParser = [] <$ eof
                   <|> ((:) <$> try parseLine <*> gradleDepTasksParser)
                   <|> (ignoreLine *> gradleDepTasksParser)
  where
  -- TODO: what are the valid characters for project names?
  parseLine = takeWhile1P Nothing (\c -> isLetter c || c == '-') <* chunk ":dependencies " <* ignoreLine
  ignoreLine = takeWhileP Nothing (\c -> c /= '\n' && c /= 'r') *> optional eol

discover :: forall r. Members '[Embed IO, Exec, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover = walk $ \dir subdirs files -> do
    let buildscripts = filter (\f -> "build.gradle" `isPrefixOf` fileName f) files

    if null buildscripts
      then walkContinue
      else do
        projects <- undefined dir :: Sem r [Text]
        if null projects
          then walkContinue
          else do
            output (configure dir projects)
            walkSkipAll subdirs

strategy :: Strategy GradleOpts
strategy = Strategy
  { strategyName = "gradle-cli"
  , strategyAnalyze = analyze
  , strategyModule = gradleOptsDir
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

analyze :: Members '[Exec, Error CLIErr] r => GradleOpts -> Sem r G.Graph
analyze GradleOpts{..} = do
  undefined
{-
  (exitcode, stdout, stderr) <- exec gradleOptsDir "gradle" [T.unpack gradleOptsProject <> ":dependencies", "--offline", "--quiet"]
  when (exitcode /= ExitSuccess) (throw $ StrategyFailed $ "Gradle returned an error: " <> BL8.unpack stderr)
  undefined
-}

configure :: Path Rel Dir -> [Text] -> ConfiguredStrategy
configure dir projects = ConfiguredStrategy strategy (GradleOpts dir projects)
