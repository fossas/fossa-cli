module Strategy.Python.SetupPy (
  analyze',
  installRequiresParserSetupCfg,
) where

import Control.Effect.Diagnostics
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Effect.ReadFS
import Graphing (Graphing)
import Path
import Strategy.Python.Util
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Types

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> Maybe (Path Abs File) -> m (Graphing Dependency)
analyze' file setupCfg = do
  reqs <- readContentsParser installRequiresParser file
  reqsFromCfg <- maybe (pure []) (readContentsParser installRequiresParserSetupCfg) setupCfg

  context "Building dependency graph" $ pure (buildGraph $ reqs ++ reqsFromCfg)

type Parser = Parsec Void Text

installRequiresParser :: Parser [Req]
installRequiresParser = do
  maybePrefix <- optional (try prefix)
  -- When we find `install_requires`, try to parse requirement strings
  case maybePrefix of
    Nothing -> pure []
    Just _ -> entries <* end
  where
    prefix = skipManyTill anySingle (symbol "install_requires") *> symbol "=" *> symbol "["
    entries = (requireSurroundedBy "\"" <|> requireSurroundedBy "\'") `sepEndBy` symbol ","

    requireSurroundedBy :: Text -> Parser Req
    requireSurroundedBy quote = between (symbol quote) (symbol quote) requirementParser

    end = symbol "]"

    symbol :: Text -> Parser Text
    symbol = L.symbol space

-- | Parses install requirements listed in setup.cfg 
-- Docs: https://setuptools.pypa.io/en/latest/userguide/declarative_config.html
installRequiresParserSetupCfg :: Parser [Req]
installRequiresParserSetupCfg = do
  maybePrefix <- optional (try prefix)
  case maybePrefix of
    Nothing -> pure []
    Just _ -> parseReqs
  where
    prefix :: Parser Text
    prefix = skipManyTill anySingle $ lookAhead parseHeader

    parseHeader :: Parser Text
    parseHeader = lexeme $ symbol "install_requires" <* chunk "="

    lineComment :: Parser ()
    lineComment = L.skipLineComment "#"

    scn :: Parser ()
    scn = L.space space1 lineComment empty

    sc :: Parser ()
    sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme sc

    symbol :: Text -> Parser Text
    symbol = L.symbol space

    parseReqs :: Parser [Req]
    parseReqs = L.nonIndented scn (L.indentBlock scn p)
      where
        p = do
          void parseHeader
          pure $ L.IndentMany Nothing pure parseReq

    parseReq :: Parser Req
    parseReq = lexeme requirementParser <?> "requirement"
