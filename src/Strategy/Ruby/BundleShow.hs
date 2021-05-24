{-# LANGUAGE RecordWildCards #-}

module Strategy.Ruby.BundleShow
  ( analyze'

  , BundleShowDep(..)
  , buildGraph
  , bundleShowParser
  )
  where

import Control.Effect.Diagnostics
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Void (Void)
import DepTypes
import Effect.Exec
import Graphing (Graphing)
import qualified Graphing
import Path
import Text.Megaparsec
import Text.Megaparsec.Char

bundleShowCmd :: Command
bundleShowCmd = Command
  { cmdName = "bundle"
  , cmdArgs = ["show"]
  , cmdAllowErr = Never
  }

analyze' :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Graphing Dependency)
analyze' dir = do
  deps <- execParser @[BundleShowDep] bundleShowParser dir bundleShowCmd
  context "Building dependency graph" $ pure (buildGraph deps)

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
