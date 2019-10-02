
module Strategy.Python.ReqTxt
  ( discover
  , strategy
  , analyze
  , configure
  )
  where

import Prologue hiding ((<?>), many, some)

import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Config
import qualified Graph as G
import           Discovery.Core
import           Discovery.Walk
import           Effect.ReadFS
import           Strategy
import           Strategy.Python.Util
import           Types

discover :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "requirements.txt") files of
    Nothing -> walkContinue
    Just file  -> do
      output (configure file)
      walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "python-requirements"
  , strategyAnalyze = analyze
  }

analyze :: Members '[Error CLIErr, ReadFS] r => BasicFileOpts -> Sem r G.Graph
analyze BasicFileOpts{..} = do
  contents <- readContentsText targetFile
  case runParser requirementsTxtParser "source" contents of
    Left err -> throw $ StrategyFailed $ "failed to parse requirements.txt " <> show err -- TODO: better error
    Right a -> pure $ buildGraph a

type Parser = Parsec Void Text

-- https://pip.pypa.io/en/stable/reference/pip_install/#requirements-file-format
requirementsTxtParser :: Parser [Req]
requirementsTxtParser = concat <$> ((line `sepBy` eol) <* eof)
  where
  isEndLine :: Char -> Bool
  isEndLine '\n' = True
  isEndLine '\r' = True
  isEndLine _    = False

  -- ignore content until the end of the line
  ignored :: Parser ()
  ignored = () <$ takeWhileP (Just "ignored") (not . isEndLine)

  comment :: Parser ()
  comment = char '#' *> ignored

  -- TODO: we can case split / sum-type this for better analysis
  line = [] <$ char '-' <* ignored -- pip options
     <|> [] <$ char '.' <* ignored -- relative path
     <|> [] <$ char '/' <* ignored -- absolute path
     <|> [] <$ oneOfS ["http:", "https:", "git+", "hg+", "svn+", "bzr+"] <* ignored -- URLs
     <|> [] <$ comment
     <|> (pure <$> requirementParser <* optional comment)
     <|> pure [] -- empty line

  oneOfS = asum . map string

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
