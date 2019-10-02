
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module Strategy.Gradle
  (
  ) where

import Prologue

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output

import           Config
import           Discovery.Walk
import           Effect.Exec
import           Effect.ReadFS
import qualified Graph as G
import           Strategy
import           Types

import Path.IO (WalkAction)

data GradleOpts = GradleOpts
  { gradleOptsDir               :: Path Rel Dir
  , gradleOptsProject           :: Text
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
  toJSON GradleOpts{..} = object ["dir" .= gradleOptsDir, "project" .= gradleOptsProject]

data Gradle m a where
  GradleProjects :: Path Rel Dir -> Gradle m [String]

makeSem ''Gradle

gradleToIO :: Member (Embed IO) r => InterpreterFor Gradle r
gradleToIO = undefined

{-
gradle :: (Member Exec r, Member ReadFS r) => Path b Dir -> [String] -> Sem r (ExitCode, BL8.ByteString, BL8.ByteString)
gradle dir args = do
  wrappers <- filterM doesFileExist
                [ dir </> [relfile|gradlew|]
                , dir </> [relfile|gradlew.bat|]
                ]

  gradlewExists <- doesFileExist (dir </> [relfile|gradlew|])
  undefined
-}

discover :: forall r. Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover = gradleToIO . walk (\dir subdirs files -> do
    let buildscripts = filter (\f -> "build.gradle" `isPrefixOf` fileName f) files

    if null buildscripts
      then walkContinue
      else do
        projects <- gradleProjects dir
        if null projects
          then walkContinue
          else do
            traverse_ (output . configure dir . T.pack) projects
            walkSkipAll subdirs
  )

strategy :: Strategy GradleOpts
strategy = Strategy
  { strategyName = "gradle-cli"
  , strategyAnalyze = analyze
  }

analyze :: Members '[Exec, Error CLIErr] r => GradleOpts -> Sem r G.Graph
analyze GradleOpts{..} = do
  (exitcode, stdout, stderr) <- exec gradleOptsDir "gradle" [T.unpack gradleOptsProject <> ":dependencies", "--offline", "--quiet"]
  when (exitcode /= ExitSuccess) (throw $ StrategyFailed $ "Gradle returned an error: " <> BL8.unpack stderr)
  undefined

configure :: Path Rel Dir -> Text -> ConfiguredStrategy
configure path project = ConfiguredStrategy strategy (GradleOpts path project)
