module Strategy.Maven.PluginTree (parseArtifact
                                 , Artifact(..), parseTextArtifact
                                 , TextArtifact(..), parseArtifactChild) where

import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, chunk, takeWhile1P, (<|>), sepBy, count, many, try)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import qualified Data.Text as Text
import Debug.Trace (traceShow)

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

data TextArtifact = TextArtifact {
  artifactText :: Text
  , scopes :: [Text]
  , children :: [TextArtifact]
  -- , isOptional :: Bool
  } deriving (Eq, Ord, Show)

parseArtifactChild :: Int -- ^ Number of recursion levels deep we are
                   -> Parser TextArtifact
parseArtifactChild level = try $
  (count level (lexeme $ char '|'))
  *> lexeme (chunk "+-"
              <|> chunk "\\-") *> (parseLevelNTextArtifact (succ level))

parseLevelNTextArtifact :: Int -> Parser TextArtifact
parseLevelNTextArtifact level =
  TextArtifact
  <$> (Text.intercalate ":" <$> count 3 (readThruNextColon "artifactSpecifier"))
  <*> scopeParse
  <*> many (parseArtifactChild level)
  
parseTextArtifact :: Parser TextArtifact
parseTextArtifact = parseLevelNTextArtifact 0
