{-# LANGUAGE RecordWildCards #-}

module Strategy.Scala.SbtDependencyTree (
  analyze,

  -- * for testing
  SbtArtifact (..),
  SbtDep (..),
  parseSbtArtifact,
  parseEviction,
  sbtTreeParser,
  removeLogPrefixes,
  buildGraph,
) where

import Control.Carrier.Simple (Has)
import Control.Effect.Diagnostics (Diagnostics, context, errCtx, fatal)
import Control.Monad (guard, void)
import Data.Functor (($>))
import Data.Maybe qualified as DMaybe
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
  AllowErr (Never),
  Command (..),
  Exec,
  ExecErr (CommandParseError),
  execThrow,
 )
import Graphing (Graphing, shrinkRoots, unfold)
import Path (Abs, Dir, Path)
import Strategy.Scala.Errors (MaybeWithoutDependencyTreeTask (MaybeWithoutDependencyTreeTask))
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
sbtDepTreeCmd :: Text -> Command
sbtDepTreeCmd projectName =
  Command
    { cmdName = "sbt"
    , cmdArgs =
        [ "--batch" -- ensure sbt does not enter repl mode!
        , "--no-colors"
        , projectName <> "/dependencyTree"
        ]
    , cmdAllowErr = Never
    }

data SbtArtifact = SbtArtifact
  { groupId :: Text
  , artifactId :: Text
  , version :: Text
  }
  deriving (Eq, Ord, Show)

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
    sbtRecurse depth = chunk "\n" *> parseDeps depth

-- | Removes log prefix from the log.
-- >> removeLogPrefixes "[info] someInfo" = "someInfo"
removeLogPrefixes :: Text -> Text
removeLogPrefixes content = Text.unlines . map removePrefix $ Text.lines content
  where
    removePrefix candidate
      | Text.isPrefixOf "[debug]" candidate = DMaybe.fromMaybe candidate $ Text.stripPrefix "[debug] " candidate
      | Text.isPrefixOf "[info]" candidate = DMaybe.fromMaybe candidate $ Text.stripPrefix "[info] " candidate
      | Text.isPrefixOf "[warn]" candidate = DMaybe.fromMaybe candidate $ Text.stripPrefix "[warn] " candidate
      | Text.isPrefixOf "[success]" candidate = DMaybe.fromMaybe candidate $ Text.stripPrefix "[success] " candidate
      | Text.isPrefixOf "[error]" candidate = DMaybe.fromMaybe candidate $ Text.stripPrefix "[error] " candidate
      | Text.isPrefixOf "[trace]" candidate = DMaybe.fromMaybe candidate $ Text.stripPrefix "[trace] " candidate
      | otherwise = candidate

buildGraph :: [SbtDep] -> Graphing Dependency
buildGraph dependencies = unfold dependencies requires toDependency
  where
    toDependency SbtDep{..} =
      Dependency
        { dependencyType = MavenType
        , dependencyName = toText (groupId artifact) <> ":" <> toText (artifactId artifact)
        , dependencyVersion = Just (CEq $ version artifact)
        , dependencyLocations = mempty
        , dependencyEnvironments = Set.singleton EnvProduction
        , dependencyTags = mempty
        }

analyze :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> Text -> m (Graphing Dependency)
analyze buildSbtDir projectName = do
  let sbtCmd = sbtDepTreeCmd projectName

  rawSbtDepTreeStdout <-
    context ("inferring dependencies for project: " <> projectName) $
      errCtx (MaybeWithoutDependencyTreeTask projectName) $
        execThrow buildSbtDir sbtCmd

  case runParser sbtTreeParser "" (removeLogPrefixes $ decodeUtf8 rawSbtDepTreeStdout) of
    Right parsedDeps -> pure $ shrinkRoots $ buildGraph parsedDeps
    Left err ->
      fatal $
        CommandParseError sbtCmd (toText $ errorBundlePretty err)
