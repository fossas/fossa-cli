
module Strategy.Python.ReqTxt
  ( discover
  , strategy
  , analyze
  , configure
  )
  where

import Prologue

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
  { discoverName = "requirements.txt"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files ->
  case find (\f -> fileName f == "requirements.txt") files of
    Nothing -> walkContinue
    Just file  -> do
      output (configure file)
      walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "python-requirements"
  , strategyAnalyze = \opts -> analyze & fileInputParser requirementsTxtParser (targetFile opts)
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = NotComplete
  }

analyze :: Member (Input [Req]) r => Sem r G.Graph
analyze = buildGraph <$> input

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

  -- FUTURE: we can case split / sum-type this for better analysis
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
