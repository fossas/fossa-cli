{-# LANGUAGE RecordWildCards #-}

module Strategy.Scala.SbtDependencyTree (
  analyze,
  sbtDepTreeCmd,

  -- * for testing
  SbtArtifact (..),
  SbtDep (..),
  parseSbtArtifact,
  parseEviction,
  sbtTreeParser,
  buildGraph,
) where

import Control.Carrier.Simple (Has)
import Control.Effect.Diagnostics (Diagnostics, fatal, fromMaybeText)
import Control.Monad (guard, void)
import Data.ByteString.Lazy (ByteString)
import Data.Functor (($>))
import Data.Set qualified as Set
import Data.String.Conversion (ConvertUtf8 (decodeUtf8), toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import DepTypes (
  DepEnvironment (EnvProduction),
  DepType (MavenType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Exec (
  Command (..),
  Exec,
  ExecErr (CommandParseError),
 )
import Graphing (Graphing, shrinkRoots, subGraphOf, unfold)
import Strategy.Scala.Common (SbtArtifact (SbtArtifact, artifactId, groupId, version), mkSbtCommand, removeLogPrefixes)
import Text.Megaparsec (
  MonadParsec (eof, takeWhileP, try),
  Parsec,
  chunk,
  empty,
  errorBundlePretty,
  many,
  optional,
  runParser,
  sepBy,
  some,
  (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char, eol)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | Sbt Dependency Ascii tree task
-- This only works with sbt v1.4.0 greater, or with sbt which has DependencyTreePlugin.
-- Ref: https://www.scala-sbt.org/1.x/docs/sbt-1.4-Release-Notes.html#sbt-dependency-graph+is+in-sourced
sbtDepTreeCmd :: Command
sbtDepTreeCmd = mkSbtCommand "dependencyTree"

data SbtDep = SbtDep
  { artifact :: SbtArtifact
  , requires :: [SbtDep]
  }
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

sc :: Parser ()
sc = Lexer.space (void $ some $ char ' ' <|> char '\t') empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: Text -> Parser Text
symbol = Lexer.symbol sc

-- | Parses package identifier.
--
-- @Examples:
-- >> parseTest parseValidProjectIdentifier "org.a.b" = "org.a.b"
-- >> parseTest parseValidProjectIdentifier "org.a.b-c" = "org.a.b-c"
-- >> parseTest parseValidProjectIdentifier "org.a.b:" = "org.a.b"
-- >> parseTest parseValidProjectIdentifier "org.a.b " = "org.a.b"
parseValidProjectIdentifier :: Parser Text
parseValidProjectIdentifier = toText <$> some (alphaNumChar <|> char '.' <|> char '-' <|> char '_')

-- | Parses Sbt Artifact
--
-- @Example:
-- >> parseTest parseSbtArtifact "com.typesafe:config:1.3.1" = SbtArtifact "com.typesafe" "config" "1.3.1"
-- >> parseTest parseSbtArtifact "com.typesafe:config:1.3.1 [S]" = SbtArtifact "com.typesafe" "config" "1.3.1"
parseSbtArtifact :: Parser SbtArtifact
parseSbtArtifact =
  SbtArtifact
    <$> parseValidProjectIdentifier
    <*> (":" >> parseValidProjectIdentifier)
    <*> (":" >> parseValidProjectIdentifier)

-- | Parses chars used for ascii graphing layout.
parseAsciiGraphLayoutChars :: Parser Text
parseAsciiGraphLayoutChars =
  toText
    <$> many
      ( char ' '
          <|> char '|'
          <|> char '+'
          <|> char '-'
          <|> char '#'
      )

-- | Parses evicted version identifier
-- >> parseEviction "(evicted by: 2.0)" = "2.0"
parseEviction :: Parser (Text)
parseEviction = symbol "(evicted by:" *> parseValidProjectIdentifier <* symbol ")"

-- | Parses ASCII tree
--
-- parent
--   +-childA
--   +-childB
--   |   +-childC
--   |
--   +-childD
--
-- Reference: https://github.com/sbt/sbt/blob/develop/main/src/main/scala/sbt/internal/SettingGraph.scala#L84
sbtTreeParser :: Parser [SbtDep]
sbtTreeParser = concat <$> ((try (parseDeps 0) <|> ignoredLine) `sepBy` eol) <* eof
  where
    isEndLine :: Char -> Bool
    isEndLine '\n' = True
    isEndLine '\r' = True
    isEndLine _ = False

    ignoredLine :: Parser [SbtDep]
    ignoredLine = void (takeWhileP (Just "ignored") (not . isEndLine)) $> mempty

    parseDeps :: Int -> Parser [SbtDep]
    parseDeps depth = do
      prefixes <- parseAsciiGraphLayoutChars
      guard (Text.length prefixes == expectedSpacing depth)
      parseDep depth <|> emptyLineBreakInGraph

    -- Refer to: https://github.com/sbt/sbt/blob/develop/main/src/main/scala/sbt/internal/SettingGraph.scala#L107
    expectedSpacing :: Int -> Int
    expectedSpacing currentDepth =
      if currentDepth <= 0
        then 0
        else 2 + currentDepth * 2

    parseDep :: Int -> Parser [SbtDep]
    parseDep depth = do
      depArtifact <- lexeme parseSbtArtifact

      -- Sbt may choose different version of dependency than specified
      -- if there are version conflicts!
      -- Ref: https://www.scala-sbt.org/0.13/docs/Library-Management.html#Eviction+warning
      evictedVersion <- optional . try $ parseEviction
      void $ takeWhileP (Just "ignored") (not . isEndLine)

      deepDependencies <- many $ try (sbtRecurse (depth + 1))

      case evictedVersion of
        Nothing -> pure [SbtDep depArtifact (concat deepDependencies)]
        Just ev -> pure [SbtDep depArtifact{version = ev} (concat deepDependencies)]

    -- Used to handles scenarios like,
    --
    --   +-childB
    --   |   +-childC
    --   |            <- emptyLineBreakInGraph
    --   +-childD
    --
    emptyLineBreakInGraph :: Parser [SbtDep]
    emptyLineBreakInGraph = mempty

    sbtRecurse :: Int -> Parser [SbtDep]
    sbtRecurse depth = (chunk "\n" <|> chunk "\r\n") *> parseDeps depth

-- | Builds graph with scoped to @SbtArtifact
--
-- Given following graph in [SbtDep] shape:
--
-- org:PROJECTA:1.0-SNAPSHOT [S]
--   +-org:B:1.0
--   | +-org:C:1.0
--   | +-org:D:1.0
--   |
--   +-org:E:1.0
--
-- org:PROJECTB:1.0-SNAPSHOT [S]
--   +-org:T:1.0
--   | +-org:U:1.0
--   |
--   +-org:W:1.0 (evicted by: 2.0)
--
-- 'buildGraph 'org:PROJECTB:1.0-SNAPSHOT' sbtDpes' return following graph:
--
-- org:PROJECTB:1.0-SNAPSHOT [S]
--   +-org:T:1.0
--   | +-org:U:1.0
--   |
--   +-org:W:1.0 (evicted by: 2.0)
buildGraph :: SbtArtifact -> [SbtDep] -> Graphing Dependency
buildGraph onlyArtifact dependencies =
  subGraphOf (toDependency onlyArtifact) $
    unfold dependencies requires (toDependency . artifact)

toDependency :: SbtArtifact -> Dependency
toDependency SbtArtifact{..} =
  Dependency
    { dependencyType = MavenType
    , dependencyName = toText groupId <> ":" <> toText artifactId
    , dependencyVersion = Just (CEq version)
    , dependencyLocations = mempty
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = mempty
    }

analyze :: (Has Exec sig m, Has Diagnostics sig m) => (Maybe ByteString) -> SbtArtifact -> m (Graphing Dependency)
analyze maybeDepTree pomArtifact = do
  depTree <- fromMaybeText "Could not retrieve output from sbt dependencyTree" maybeDepTree
  case runParser sbtTreeParser "" (removeLogPrefixes $ decodeUtf8 depTree) of
    Right parsedDeps -> pure $ shrinkRoots $ buildGraph pomArtifact parsedDeps
    Left err -> fatal $ CommandParseError sbtDepTreeCmd (toText $ errorBundlePretty err)
