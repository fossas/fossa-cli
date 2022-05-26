module Strategy.Maven.PluginTree (
  parseArtifact,
  Artifact (..),
  parseTextArtifact,
  TextArtifact (..),
  parseArtifactChild,
) where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec, chunk, count, eof, getSourcePos, sepBy, some, sourceColumn, takeWhile1P, takeWhileP, try, unPos, (<|>))
import Text.Megaparsec.Char (char, space1, string)
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
lexeme = Lexer.lexeme (space1 <|> eof <|> fail "Failed to parse lexeme")

readThruNextColon :: String -> Parser Text
readThruNextColon name = takeWhile1P (Just name) (/= ':') <* char ':'

scopeParse :: Parser [Text]
scopeParse = sepBy (takeWhile1P (Just "scopes") (\c -> not (isSpace c || c == '/'))) (char '/')

parseIsOptional :: Parser Bool
parseIsOptional =
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
    <*> parseIsOptional

data TextArtifact = TextArtifact
  { artifactText :: Text
  , scopes :: [Text]
  , isOptional :: Bool
  , children :: [TextArtifact]
  }
  deriving (Eq, Ord, Show)

currentColumn :: Parser Int
currentColumn = unPos . sourceColumn <$> getSourcePos

parseArtifactChild ::
  -- | Column where we expect to be able to parse a child artifact
  Int ->
  Parser TextArtifact
parseArtifactChild prefixCount =
  try $ do
    void $ takeWhileP Nothing (\c -> c `notElem` ['\\', '+'])
    pos <- currentColumn
    if pos == prefixCount
      then
        lexeme (string "+-" <|> string "\\-")
          *> parseTextArtifactAndChildren
      else fail "can't parse child"

parseTextArtifactAndChildren :: Parser TextArtifact
parseTextArtifactAndChildren =
  do
    startPos <- currentColumn
    TextArtifact
      <$> (Text.intercalate ":" <$> count 3 (readThruNextColon "artifactSpecifier"))
      <*> lexeme scopeParse
      <*> parseIsOptional
      <*> ( some (parseArtifactChild startPos)
              <|> pure []
          )

parseTextArtifact :: Parser TextArtifact
parseTextArtifact = parseTextArtifactAndChildren <* eof
