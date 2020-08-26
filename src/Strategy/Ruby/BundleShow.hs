{-# LANGUAGE RecordWildCards #-}

module Strategy.Ruby.BundleShow
  ( discover
  , analyze

  , BundleShowDep(..)
  , buildGraph
  , bundleShowParser
  )
  where

import Control.Effect.Diagnostics
import Data.Foldable (find)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Void (Void)
import DepTypes
import Discovery.Walk
import Effect.Exec
import Graphing (Graphing)
import qualified Graphing
import Path
import Text.Megaparsec
import Text.Megaparsec.Char
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \dir _ files -> do
  case find (\f -> fileName f `elem` ["Gemfile", "Gemfile.lock"]) files of
    Nothing -> pure ()
    Just _  -> runSimpleStrategy "ruby-bundleshow" RubyGroup $ analyze dir

  pure WalkContinue

bundleShowCmd :: Command
bundleShowCmd = Command
  { cmdName = "bundle"
  , cmdArgs = ["show"]
  , cmdAllowErr = Never
  }

analyze :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m ProjectClosureBody
analyze dir = mkProjectClosure dir <$> execParser bundleShowParser dir bundleShowCmd

mkProjectClosure :: Path Abs Dir -> [BundleShowDep] -> ProjectClosureBody
mkProjectClosure dir deps = ProjectClosureBody
  { bodyModuleDir    = dir
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph deps
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

buildGraph :: [BundleShowDep] -> Graphing Dependency
buildGraph = Graphing.fromList . map toDependency
  where
  toDependency BundleShowDep{..} =
    Dependency { dependencyType = GemType
               , dependencyName = depName
               , dependencyVersion = Just (CEq depVersion)
               , dependencyLocations = []
               , dependencyEnvironments = []
               , dependencyTags = M.empty
               }

data BundleShowDep = BundleShowDep
  { depName    :: Text
  , depVersion :: Text
  } deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

bundleShowParser :: Parser [BundleShowDep]
bundleShowParser = concat <$> ((line <|> ignoredLine) `sepBy` eol) <* eof
  where
  isEndLine :: Char -> Bool
  isEndLine '\n' = True
  isEndLine '\r' = True
  isEndLine _    = False

  -- ignore content until the end of the line
  ignored :: Parser ()
  ignored = () <$ takeWhileP (Just "ignored") (not . isEndLine)

  ignoredLine :: Parser [BundleShowDep]
  ignoredLine = do
    ignored
    pure []

  findDep :: Parser Text
  findDep = takeWhileP (Just "dep") (/= ' ')

  findVersion :: Parser Text
  findVersion = takeWhileP (Just "version") (/= ')')

  line :: Parser [BundleShowDep]
  line = do
    _ <- chunk "  * "
    dep <- findDep
    _ <- chunk " ("
    version <- findVersion
    _ <- char ')'
    pure [BundleShowDep dep version]
