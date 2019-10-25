
{-# language QuasiQuotes #-}

module Strategy.Ruby.BundleShow
  ( discover
  , strategy
  , analyze
  , configure

  , BundleShowDep(..)
  , buildGraph
  , bundleShowParser
  )
  where

import Prologue hiding (many)

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Discovery.Walk
import           Effect.Exec
import           Effect.GraphBuilder
import qualified Graph as G
import           Types

discover :: Discover
discover = Discover
  { discoverName = "bundleshow"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \dir _ files ->
  case find (\f -> fileName f `elem` ["Gemfile", "Gemfile.lock"]) files of
    Nothing -> walkContinue
    Just _  -> do
      output (configure dir)
      walkContinue

bundleShowCmd :: Command
bundleShowCmd = Command
  { cmdNames = ["bundle"]
  , cmdBaseArgs = ["show"]
  , cmdAllowErr = Never
  }

strategy :: Strategy BasicDirOpts
strategy = Strategy
  { strategyName = "ruby-bundleshow"
  , strategyAnalyze = \opts -> analyze & execInputParser bundleShowParser (targetDir opts) bundleShowCmd []
  , strategyModule = targetDir
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

analyze :: Member (Input [BundleShowDep]) r => Sem r G.Graph
analyze = buildGraph <$> input

buildGraph :: [BundleShowDep] -> G.Graph
buildGraph xs = unfold xs (const []) toDependency
  where
  toDependency BundleShowDep{..} =
    G.Dependency { dependencyType = G.GemType
                 , dependencyName = depName
                 , dependencyVersion = Just (G.CEq depVersion)
                 , dependencyLocations = []
                 , dependencyTags = M.empty
                 }

configure :: Path Rel Dir -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicDirOpts

data BundleShowDep = BundleShowDep
  { depName    :: Text
  , depVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

type Parser = Parsec Void Text

ignored :: Parser ()
ignored = () <$ takeWhileP (Just "ignored") (not . isEndLine)

ignoredLine :: Parser [BundleShowDep]
ignoredLine = do
  ignored
  pure $ []

isEndLine :: Char -> Bool
isEndLine '\n' = True
isEndLine '\r' = True
isEndLine _    = False

bundleShowParser :: Parser [BundleShowDep]
bundleShowParser = concat <$> ((line <|> ignoredLine) `sepBy` eol) <* eof
  where

  -- ignore content until the end of the line

  findDep :: Parser Text
  findDep = takeWhileP (Just "dep") (\a -> a /= ' ')

  findVersion :: Parser Text
  findVersion = takeWhileP (Just "version") (\a -> a /= ')')

  -- TODO: we can case split / sum-type this for better analysis
  line :: Parser [BundleShowDep]
  line = do
    chunk "  * "
    dep <- findDep
    chunk " ("
    version <- findVersion
    char ')'
    pure $ [BundleShowDep dep version]


  -- oneOfS = asum . map string
