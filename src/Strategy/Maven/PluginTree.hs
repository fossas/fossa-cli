module Strategy.Maven.PluginTree (
  parseTextArtifact,
  parseTextArtifacts,
  TextArtifact (..),
  parseArtifactChild,
) where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tree (Tree (Node))
import Data.Void (Void)
import Text.Megaparsec (
  Parsec,
  chunk,
  eof,
  getSourcePos,
  many,
  satisfy,
  sepBy,
  some,
  sourceColumn,
  takeWhile1P,
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
    -- Connector prefixes consist only of continuation bars and whitespace.
    -- Any other text (e.g. an unconnected artifact line that starts a new
    -- module's block in multi-module output) is not a child of this parent;
    -- failing here lets the top level parse it as a new root instead of
    -- swallowing the whole following block as one giant prefix.
    void $ many (satisfy (\c -> c `elem` ['|', ' ', '\t', '\n']))
    pos <- currentColumn
    if pos == prefixCount
      then -- The parser is where we expect a child of its parent to be positioned
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

-- | Parse output from the maven depgraph plugin.
--
-- The output of the depgraph plugin looks like:
--
-- @
-- org.clojure:clojure:1.12.0-master-SNAPSHOT:compile
-- +- org.clojure:spec.alpha:0.3.218:compile
-- +- org.clojure:core.specs.alpha:0.2.62:compile
-- +- org.clojure:test.generative:1.0.0:test
-- |  +- org.clojure:tools.namespace:1.0.0:test/compile
-- |  |  +- org.clojure:java.classpath:1.0.0:test
-- |  |  \- org.clojure:tools.reader:1.3.2:test
-- |  \- org.clojure:data.generators:1.0.0:test (optional)
-- +- org.clojure:test.check:1.1.1:test
-- \- javax.xml.ws:jaxws-api:2.3.0:test
--    +- javax.xml.bind:jaxb-api:2.3.0:test
--    \- javax.xml.soap:javax.xml.soap-api:1.4.0:test
-- @
--
-- The strings after the tree structure text (e.g. '+-', '|', '\-') represent
-- a maven artifact and have the format
-- @<groupId>:<artifactId>:<version>:<slash delimited scopes> (optional)@
-- The final '(optional)' string may or may not be present.
--
-- The parser should parse the text into a tree of these artifacts.
parseTextArtifact :: Parser (Tree TextArtifact)
parseTextArtifact = parseTextArtifactAndChildren True <* eof

-- | Parse plugin output containing one top-level tree per reactor module.
--
-- The @aggregate@ goal emits a separate root block for every module of the
-- build, so a multi-module project yields several trees in one file (and the
-- same dependency can repeat under different roots). 'parseTextArtifact' only
-- accepts a single tree and fails on real multi-module output.
parseTextArtifacts :: Parser [Tree TextArtifact]
parseTextArtifacts = do
  first <- parseTextArtifactAndChildren True
  -- Each following root block must start at column 0 after whitespace; the
  -- 'try' rolls back the consumed separator when a block is not a valid root,
  -- so trailing garbage still fails the final 'eof' instead of being dropped.
  rest <- many $ try nextRootBlock
  void $ many (satisfy isSpace)
  eof
  pure (first : rest)
  where
    -- 'many', not 'some': the previous block's trailing lexeme has usually
    -- already consumed the separating newline, leaving zero whitespace before
    -- the next root starts at column 0.
    nextRootBlock :: Parser (Tree TextArtifact)
    nextRootBlock = do
      void $ many (satisfy isSpace)
      parseTextArtifactAndChildren True
