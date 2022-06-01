module Strategy.Maven.PluginTree (
  parseTextArtifact,
  TextArtifact (..),
  parseArtifactChild,
  foldTextArtifactl,
  foldTextArtifactM,
) where

import Control.Monad (foldM, void)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (
  Parsec,
  chunk,
  count,
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
import Data.List (foldl')

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
  , scopes :: [Text]
  , isOptional :: Bool
  , children :: [TextArtifact]
  }
  deriving (Eq, Ord, Show)

foldTextArtifactl :: (a -> TextArtifact -> a) -> a -> TextArtifact -> a
foldTextArtifactl f a t@(TextArtifact{children = children}) =
  inner `seq` foldl' (foldTextArtifactl f) inner children
  where inner = f a t

foldTextArtifactM :: Monad m => (a -> TextArtifact -> m a) -> a -> TextArtifact -> m a
foldTextArtifactM f a t@TextArtifact{children = children} = do
  res <- f a t
  foldM (foldTextArtifactM f) res children

artifactWithChildren :: TextArtifact
artifactWithChildren =
  TextArtifact
    { artifactText = "org.clojure:test.generative:1.0.0"
    , scopes = ["test"]
    , isOptional = False
    , children =
        [ TextArtifact
            { artifactText = "org.clojure:tools.namespace:1.0.0"
            , scopes = ["test"]
            , isOptional = False
            , children =
                [ TextArtifact
                    { artifactText = "org.clojure:java.classpath:1.0.0"
                    , scopes = ["test"]
                    , children = []
                    , isOptional = False
                    }
                , TextArtifact
                    { artifactText = "org.fake:fake-pkg:1.0.0"
                    , scopes = ["compile"]
                    , children = []
                    , isOptional = True
                    }
                , TextArtifact
                    { artifactText = "org.clojure:tools.reader:1.3.2"
                    , scopes = ["test"]
                    , children = []
                    , isOptional = False
                    }
                ]
            }
        , TextArtifact
            { artifactText = "org.foo:bar:1.0.0"
            , scopes = ["compile"]
            , isOptional = False
            , children =
                [ TextArtifact
                    { artifactText = "org.baz:buzz:1.0.0"
                    , scopes = ["test"]
                    , children = []
                    , isOptional = False
                    }
                ]
            }
        ]
    }

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
