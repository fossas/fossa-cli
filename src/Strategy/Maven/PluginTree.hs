module Strategy.Maven.PluginTree (parseArtifact
                                 , Artifact(..)) where

import Data.Char (isSpace)
import Data.Functor (($>))
import Data.List (singleton)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, chunk, takeWhile1P, (<|>))
import Text.Megaparsec.Char (char, space1)
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

parseArtifact :: Parser Artifact
parseArtifact =
  Artifact
    <$> readThruNextColon "groupId"
    <*> readThruNextColon "artifactId"
    <*> readThruNextColon "artifactVersion"
    -- based on  the maven documentation, it  seems there can be  only one scope
    -- for a given rtifact even though maven-depgraph-plugin returns an array
    -- of strings in the json output
    <*> (singleton <$> scopeParse)
    <*> isOptional
  where
    readThruNextColon :: String -> Parser Text
    readThruNextColon name = takeWhile1P (Just name) (/= ':') <* char ':'

    scopeParse :: Parser Text
    scopeParse = lexeme (takeWhile1P (Just "scopes") (not . isSpace))

    isOptional :: Parser Bool
    isOptional =
      (chunk "(optional)" $> True)
        <|> pure False
