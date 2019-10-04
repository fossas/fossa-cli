module Strategy.Python.SetupPy
  ( discover
  , strategy
  , analyze
  , configure
  )
  where

import Prologue hiding ((<?>), many, some)

import           Data.Char
import qualified Data.Text as T
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Text.Megaparsec
import           Text.Megaparsec.Char

import qualified Graph as G
import           Discovery.Walk
import           Effect.ReadFS
import           Strategy.Python.Util
import           Types

discover :: Discover
discover = Discover
  { discoverName = "setup.py"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (\f -> fileName f == "setup.py") files of
    Nothing -> walkContinue
    Just file  -> do
      output (configure file)
      walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "python-setuppy"
  , strategyAnalyze = analyze
  , strategyModule = parent . targetFile
  }

analyze :: Members '[Error CLIErr, ReadFS] r => BasicFileOpts -> Sem r G.Graph
analyze BasicFileOpts{..} = do
  contents <- readContentsText targetFile
  let noWhitespace = T.filter (not . isSpace) contents
  case runParser installRequiresParser "source" noWhitespace of
    Left err -> throw $ StrategyFailed $ "failed to parse setup.py " <> show err -- TODO: better error
    Right a -> pure $ buildGraph a

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
