module Strategy.Maven.PluginTree (
  parseArtifact,
  Artifact (..),
  parseTextArtifact,
  TextArtifact (..),
  parseArtifactChild,
) where

import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Debug.Trace (traceShow)
import Text.Megaparsec (Parsec, chunk, count, getSourcePos, many, sepBy, sourceColumn, takeP, takeWhile1P, try, unPos, (<|>))
import Text.Megaparsec.Char (char, space1, spaceChar)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

data Artifact = Artifact
  { artifactGroupId :: Text
  , artifactArtifactId :: Text
  , artifactVersion :: Text
  , artifactScopes :: [Text]
  , artifactOptional :: Bool
  }
  deriving (Show, Eq, Ord)

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space1

readThruNextColon :: String -> Parser Text
readThruNextColon name = takeWhile1P (Just name) (/= ':') <* char ':'

scopeParse :: Parser [Text]
scopeParse = lexeme $ sepBy (takeWhile1P (Just "scopes") (\c -> not (isSpace c || c == '/'))) (char '/')

isOptional :: Parser Bool
isOptional =
  (chunk "(optional)" $> True)
    <|> pure False

-- TODO: Do we actually need this?
parseArtifact :: Parser Artifact
parseArtifact =
  Artifact
    <$> readThruNextColon "groupId"
    <*> readThruNextColon "artifactId"
    <*> readThruNextColon "artifactVersion"
    <*> scopeParse
    <*> isOptional

data TextArtifact = TextArtifact
  { artifactText :: Text
  , scopes :: [Text]
  , children :: [TextArtifact]
  -- , isOptional :: Bool
  }
  deriving (Eq, Ord, Show)

parseArtifactChild ::
  -- | Number of characters to consume to get to the level of this parse
  Int ->
  Parser TextArtifact
parseArtifactChild level =
  try $
    takeP (Just "Artifact prefix") level
      *> lexeme (chunk "+-" <|> chunk "\\-")
      *> parseLevelNTextArtifact

-- old version which tried to count abstract levels of indentation.
-- ((count level (lexeme $ char '|'))
--  <|>
-- (count (level * 3) spaceChar))

--             <|> chunk "\\-") *> (parseLevelNTextArtifact (succ level))

parseLevelNTextArtifact :: Parser TextArtifact
parseLevelNTextArtifact =
  do startPos <- unPos . sourceColumn <$> getSourcePos
     TextArtifact
       <$> (Text.intercalate ":" <$> count 3 (readThruNextColon "artifactSpecifier"))
       <*> scopeParse
       <*> many (parseArtifactChild (startPos - 1))

parseTextArtifact :: Parser TextArtifact
parseTextArtifact = parseLevelNTextArtifact
