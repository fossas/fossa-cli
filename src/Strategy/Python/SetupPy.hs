module Strategy.Python.SetupPy (
  analyze',
  installRequiresParserSetupCfg,
  installRequiresParser,
) where

import Control.Effect.Diagnostics
import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Effect.ReadFS
import Graphing (Graphing)
import Path
import Strategy.Python.Pip (Package (..))
import Strategy.Python.Util
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Types

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Maybe [Package] -> Path Abs File -> Maybe (Path Abs File) -> m (Graphing Dependency)
analyze' packages setupPy setupCfg = do
  (pyReqs, pyPackageName) <- readContentsParser installRequiresParser setupPy
  (cfgReqs, cfgPackageName) <- maybe (pure ([], Nothing)) (readContentsParser installRequiresParserSetupCfg) setupCfg
  let pkgNames = catMaybes [pyPackageName, cfgPackageName]
  context "Building dependency graph" $ pure (buildGraphSetupPy packages pkgNames (pyReqs ++ cfgReqs))

type Parser = Parsec Void Text

data Param
  = Name
  | Requires
  deriving (Eq, Ord, Show)

installRequiresParser :: Parser ([Req], Maybe Text)
installRequiresParser = do
  maybeParam <- optional . try $ prefix (name <|> installRequires)
  case maybeParam of
    Nothing -> do
      pure ([], Nothing)
    Just Requires -> do
      requires <- entriesParser
      n <- findName
      pure (requires, n)
    Just Name -> do
      n <- optional nameParser
      reqs <- findRequires
      pure (reqs, n)

  where
    findName :: Parser (Maybe Text)
    findName = do
      maybeName <- optional . try $ prefix name
      case maybeName of
        Just _ -> Just <$> nameParser
        Nothing -> pure Nothing

    findRequires :: Parser [Req]
    findRequires = do
      maybeRequires <- optional . try $ prefix installRequires
      case maybeRequires of
        Nothing -> pure []
        Just _ -> entriesParser

    nameParser :: Parser Text
    nameParser = toText <$> lexeme
      ((between (symbol' "\"") (symbol "\"") packageName
      <|> (between (symbol "\'") (symbol "\'") packageName)))


    installRequires :: Parser Param
    installRequires = do
      void $ (symbol "install_requires") *> symbol "=" *> (symbol' "[")
      pure Requires

    name :: Parser Param
    name = do
      void $ (symbol "name") *> symbol "="
      pure Name

    prefix :: Parser Param -> Parser Param
    prefix = skipManyTill anySingle

    entriesParser :: Parser [Req]
    entriesParser = entries `sepEndBy` (symbol' ",") <* symbol "]"
      where
      entries :: Parser Req
      entries = lexeme (requireSurroundedBy "\"" <|> requireSurroundedBy "\'")

      requireSurroundedBy :: Text -> Parser Req
      requireSurroundedBy quote = between (symbol quote) (symbol quote) requirementParser

    ignoreBackslash :: Parser ()
    ignoreBackslash = void $ symbol "\\"

    symbol :: Text -> Parser Text
    symbol = L.symbol space

    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme $ L.space (space1 <|> ignoreBackslash) (L.skipLineComment "#") empty

    symbol' :: Text -> Parser Text
    symbol' = lexeme . symbol

-- Parses a python package name using regex ^([A-Z0-9]|[A-Z0-9][A-Z0-9._-]*[A-Z0-9])$
packageName :: ParsecT Void Text Identity [Char]
packageName = (:) <$> alphaNumChar <*> (concat <$> many nameEnd)
  where
    nameEnd :: ParsecT Void Text Identity [Char]
    nameEnd = pure <$> alphaNumChar <|> do
      special <- many (satisfy (\x -> x == '.' || x == '_' || x == '-'))
      lod <- alphaNumChar
      pure (special ++ [lod])
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
installRequiresParserSetupCfg :: Parser ([Req], Maybe Text)
installRequiresParserSetupCfg = do
  maybeParameter <- optional . try $ prefix (lookAhead installRequiresHeader <|> name)
  case maybeParameter of
    Nothing -> pure ([], Nothing)
    Just Requires -> do
      req <- parseReqs
      n <- findName
      pure (req, n)
    Just Name -> do
      n <- nameParser
      req <- findRequires
      pure (req, Just n)
  where
    prefix :: Parser Param -> Parser Param
    prefix = skipManyTill anySingle

    installRequiresHeader = do
      void $ lexeme $ symbol "install_requires" <* chunk "="
      pure Requires

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
          void installRequiresHeader
          pure $ L.IndentMany Nothing pure parseReq

    parseReq :: Parser Req
    parseReq = lexeme requirementParser <?> "requirement"

    findName :: Parser (Maybe Text)
    findName = do
      maybeName <- optional . try $ prefix name
      case maybeName of
        Just _ -> Just <$> nameParser
        Nothing -> pure Nothing

    findRequires :: Parser [Req]
    findRequires = do
      maybeRequires <- optional . try $ prefix $ lookAhead installRequiresHeader
      case maybeRequires of
        Nothing -> pure []
        Just _ -> parseReqs

    nameParser :: Parser Text
    nameParser = toText <$> lexeme packageName <?> "name"

    name :: Parser Param
    name = do
      void $ (symbol "name") *> symbol "="
      pure Name
