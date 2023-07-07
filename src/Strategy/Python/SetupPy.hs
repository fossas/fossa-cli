module Strategy.Python.SetupPy (
  analyze',
  installRequiresParserSetupCfg,
  installRequiresParser,
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
    prefix :: Parser Text
    prefix = skipManyTill anySingle (symbol "install_requires") *> symbol "=" *> (symbol' "[")

    entries :: Parser [Req]
    entries = entriesParser `sepEndBy` (symbol' ",")

    entriesParser :: Parser Req
    entriesParser = lexeme (requireSurroundedBy "\"" <|> requireSurroundedBy "\'")

    requireSurroundedBy :: Text -> Parser Req
    requireSurroundedBy quote = between (symbol quote) (symbol quote) requirementParser

    end :: Parser Text
    end = symbol "]"

    ignoreBackslash :: Parser ()
    ignoreBackslash = void $ symbol "\\"

    symbol :: Text -> Parser Text
    symbol = L.symbol space

    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme $ L.space (space1 <|> ignoreBackslash) (L.skipLineComment "#") empty

    symbol' :: Text -> Parser Text
    symbol' = lexeme . symbol

-- | Parses install requirements listed in setup.cfg
-- Setup.cfg has install_requires attribute in [options] block
-- which is formatted as danfling list.
-- -
-- Example setup.cfg's install_requires block
-- -
-- > install_requires =
-- >     importlib-metadata; python_version<"3.8"
-- >     # platformdirs>=2
-- >     configupdater>=3.0  # some comment
-- >     packaging>=20.7
-- >     colorama>=0.4.4; sys_platform == "win32"
-- >
-- >
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
