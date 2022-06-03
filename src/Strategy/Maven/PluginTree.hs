module Strategy.Maven.PluginTree (
  parseTextArtifact,
  TextArtifact (..),
  parseArtifactChild,
) where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tree
import Data.Void (Void)
import Text.Megaparsec (
  Parsec,
  chunk,
  eof,
  getSourcePos,
  sepBy,
  some,
  sourceColumn,
  takeWhile1P,
  takeWhileP,
  try,
  unPos,
  (<|>),
 )
import Text.Megaparsec.Char (char, space1, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

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

data TextArtifact = TextArtifact
  { artifactText :: Text
  , groupId :: Text
  , artifactId :: Text
  , textArtifactVersion :: Text
  , scopes :: [Text]
  , isOptional :: Bool
  , isDirect :: Bool
  }
  deriving (Eq, Ord, Show)

currentColumn :: Parser Int
currentColumn = unPos . sourceColumn <$> getSourcePos

parseArtifactChild ::
  -- | Column where we expect to be able to parse a child artifact
  Int ->
  Parser (Tree TextArtifact)
parseArtifactChild prefixCount =
  try $ do
    void $ takeWhileP Nothing (\c -> c `notElem` ['\\', '+'])
    pos <- currentColumn
    if pos == prefixCount
      then
        lexeme (string "+-" <|> string "\\-")
          *> parseTextArtifactAndChildren False
      else fail "can't parse child"

parseTextArtifactAndChildren :: Bool -> Parser (Tree TextArtifact)
parseTextArtifactAndChildren isDirect =
  do
    startPos <- currentColumn
    (groupId, artifactId, artifactVersion) <-
      (,,)
        <$> readThruNextColon "groupId"
        <*> readThruNextColon "artifactId"
        <*> readThruNextColon "artifactVersion"
    Node
      <$> ( TextArtifact
              (Text.intercalate ":" [groupId, artifactId, artifactVersion])
              groupId
              artifactId
              artifactVersion
              <$> lexeme scopeParse
              <*> parseIsOptional
              <*> pure (isDirect)
          )
      <*> ( some (parseArtifactChild startPos)
              <|> pure []
          )

parseTextArtifact :: Parser (Tree TextArtifact)
parseTextArtifact = parseTextArtifactAndChildren True <* eof
