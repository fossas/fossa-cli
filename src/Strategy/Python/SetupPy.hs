module Strategy.Python.SetupPy
  ( discover
  , strategy
  , analyze
  , configure
  )
  where

import Prologue hiding ((<?>), many, some)

import           Polysemy
import           Polysemy.Input
import           Polysemy.Output
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Discovery.Walk
import           Effect.ReadFS
import qualified Graph as G
import           Strategy.Python.Util
import           Types

discover :: Discover
discover = Discover
  { discoverName = "setup.py"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files ->
  case find (\f -> fileName f == "setup.py") files of
    Nothing -> walkContinue
    Just file  -> do
      output (configure file)
      walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "python-setuppy"
  , strategyAnalyze = \opts ->
      analyze & fileInputParser installRequiresParser (targetFile opts)
  , strategyModule = parent . targetFile
  }

analyze :: Member (Input [Req]) r => Sem r G.Graph
analyze = buildGraph <$> input

type Parser = Parsec Void Text

installRequiresParser :: Parser [Req]
installRequiresParser = prefix *> entries <* end
  where
  prefix  = skipManyTill anySingle (string "install_requires=[")
  entries = between quote quote requirementParser `sepBy` char ','
  end     = char ']'

  quote   = char '\''


configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
