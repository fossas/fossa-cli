module Strategy.Python.DependencyParser
  ( DependencySource(..)
  , VersionConstraint(..)
  , parseDependencySource
  , parseVersionConstraint
  , dependencySourceParser
  , gitSourceParser
  , httpSourceParser
  , fileSourceParser
  , simpleSourceParser
  , versionConstraintParser
  ) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- | Type representing the source of a dependency specification
data DependencySource
  = GitSource Text (Maybe Text) -- URL and optional reference
  | HttpSource Text             -- URL
  | FileSource Text             -- File path
  | SimpleSource Text           -- Simple package name/version
  deriving (Eq, Ord, Show)

-- | Type representing a version constraint
data VersionConstraint
  = VersionEq Text         -- == version
  | VersionGt Text         -- > version
  | VersionGtEq Text       -- >= version
  | VersionLt Text         -- < version
  | VersionLtEq Text       -- <= version
  | VersionCompatible Text -- ~= version or ^ version
  | VersionNot Text        -- != version
  | VersionWildcard        -- *
  | VersionAnd VersionConstraint VersionConstraint -- version AND version
  deriving (Eq, Ord, Show)

-- | Parse a dependency specification into a strongly typed DependencySource
parseDependencySource :: Text -> Either (ParseErrorBundle Text Void) DependencySource
parseDependencySource = runParser dependencySourceParser ""

-- | Type alias for our parser
type Parser = Parsec Void Text

-- | Parser for different dependency source types
dependencySourceParser :: Parser DependencySource
dependencySourceParser = 
      gitSourceParser 
  <|> httpSourceParser 
  <|> fileSourceParser
  <|> simpleSourceParser

-- | Parser for Git dependency sources
gitSourceParser :: Parser DependencySource
gitSourceParser = do
  _ <- string "git+"
  url <- takeWhileP (Just "git URL") (/= '@')
  reference <- optional (char '@' *> takeWhileP (Just "git reference") (/= ' '))
  return $ GitSource url reference

-- | Parser for HTTP/HTTPS dependency sources
httpSourceParser :: Parser DependencySource
httpSourceParser = do
  protocol <- string "http://" <|> string "https://"
  rest <- takeWhileP (Just "url") (/= ' ')
  return $ HttpSource (protocol <> rest)

-- | Parser for file/path dependency sources
fileSourceParser :: Parser DependencySource
fileSourceParser = fileWithScheme <|> relativePath <|> absolutePath
  where
    fileWithScheme = do
      _ <- string "file:"
      path <- takeWhileP (Just "file path") (/= ' ')
      -- Normalize file:/// URLs to remove the protocol prefix completely
      -- When the path starts with multiple slashes, keep just one
      return $ case path of
        -- If it starts with multiple slashes, extract the path portion
        p | "//" `Text.isPrefixOf` p -> 
            let cleanPath = Text.dropWhile (== '/') (Text.drop 2 p)
            in FileSource ("/" <> cleanPath)
        -- Otherwise treat as-is
        _ -> FileSource path

    relativePath = do
      prefix <- string "./" <|> string "../"
      rest <- takeWhileP (Just "relative path") (/= ' ')
      return $ FileSource (prefix <> rest)

    absolutePath = do
      _ <- char '/'
      rest <- takeWhileP (Just "absolute path") (/= ' ')
      return $ FileSource ("/" <> rest)

-- | Parser for simple package name/version specification
simpleSourceParser :: Parser DependencySource
simpleSourceParser = do
  spec <- takeWhileP (Just "simple source") (/= ' ')
  return $ SimpleSource spec
    
-- | Parse a version constraint into a strongly typed VersionConstraint
parseVersionConstraint :: Text -> Either (ParseErrorBundle Text Void) VersionConstraint
parseVersionConstraint = runParser versionConstraintParser ""

-- | Parser for version constraints
versionConstraintParser :: Parser VersionConstraint
versionConstraintParser = try andConstraintParser <|> singleConstraintParser

-- | Parser for AND-combined version constraints (e.g., ">=1.0.0,<2.0.0")
andConstraintParser :: Parser VersionConstraint
andConstraintParser = do
  first <- singleConstraintParser
  _ <- char ','
  space
  rest <- versionConstraintParser
  return $ VersionAnd first rest

-- | Parser for a single version constraint
singleConstraintParser :: Parser VersionConstraint
singleConstraintParser = do
  space
  constraint <- choice
    [ VersionCompatible <$> (string "^" *> takeVersion)
    , VersionCompatible <$> (string "~=" *> takeVersion)
    , VersionGtEq <$> (string ">=" *> takeVersion)
    , VersionGt <$> (string ">" *> takeVersion)
    , VersionLtEq <$> (string "<=" *> takeVersion)
    , VersionLt <$> (string "<" *> takeVersion)
    , VersionEq <$> (string "==" *> takeVersion)
    , VersionNot <$> (string "!=" *> takeVersion)
    , VersionWildcard <$ string "*"
    , VersionEq <$> takeVersion  -- Default to equality for bare versions
    ]
  space
  return constraint
  where
    takeVersion = takeWhileP (Just "version") (\c -> c /= ',' && c /= ' ' && c /= '\t')