module Strategy.Bazel.ModuleBazel (
  BazelModuleFile (..),
  BazelDep (..),
  ModuleInfo (..),
  moduleBazelParser,
  buildBazelGraph,
) where

import Control.Applicative (empty)
import Data.Foldable (asum)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import DepTypes (
  DepType (BazelType, MavenType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Graphing (Graphing)
import Graphing qualified
import Text.Megaparsec (
  MonadParsec (eof, lookAhead, notFollowedBy, try),
  Parsec,
  anySingle,
  between,
  choice,
  many,
  optional,
  sepEndBy,
  skipMany,
  skipManyTill,
  (<|>),
 )
import Text.Megaparsec.Char (
  alphaNumChar,
  char,
  letterChar,
  space1,
 )
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

-- | Space consumer that skips whitespace and # line comments.
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

-- | Parse a lexeme, consuming trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a symbol, consuming trailing whitespace.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Data types for MODULE.bazel parse results.
data BazelModuleFile = BazelModuleFile
  { moduleInfo :: Maybe ModuleInfo
  , bazelDeps :: [BazelDep]
  , mavenArtifacts :: [Text]
  , mavenRepositories :: [Text]
  }
  deriving (Eq, Ord, Show)

data ModuleInfo = ModuleInfo
  { moduleName :: Text
  , moduleVersion :: Text
  }
  deriving (Eq, Ord, Show)

data BazelDep = BazelDep
  { depName :: Text
  , depVersion :: Text
  , depRepoName :: Maybe Text
  }
  deriving (Eq, Ord, Show)

-- Internal types for parsing
data Statement
  = StmtModule ModuleInfo
  | StmtBazelDep BazelDep
  | StmtVarAssign Text StarlarkValue
  | StmtUseExtension Text Text
  -- ^ variable name, extension label
  | StmtExtensionCall Text Text [(Text, StarlarkValue)]
  -- ^ variable name, method name, keyword args
  | StmtOther
  deriving (Eq, Ord, Show)

data StarlarkValue
  = SString Text
  | SList [Text]
  | SBool Bool
  | SInt Int
  | SIdentifier Text
  deriving (Eq, Ord, Show)

-- | Parse the entire MODULE.bazel file.
moduleBazelParser :: Parser BazelModuleFile
moduleBazelParser = do
  sc
  stmts <- many (statement <* sc)
  eof
  let vars = Map.fromList [(n, v) | StmtVarAssign n v <- stmts]
      extVars = Map.fromList [(n, lbl) | StmtUseExtension n lbl <- stmts]
      modInfo = firstOf [m | StmtModule m <- stmts]
      deps = [d | StmtBazelDep d <- stmts]
      extCalls = [(v, method, args) | StmtExtensionCall v method args <- stmts]
      mavenExts = Map.keysSet $ Map.filter isMavenExt extVars
      mavenCalls = [(method, args) | (v, method, args) <- extCalls, Set.member v mavenExts]
      artifacts = concatMap (extractMavenArtifacts vars) mavenCalls
      repos = concatMap (extractMavenRepos vars) mavenCalls
  pure
    BazelModuleFile
      { moduleInfo = modInfo
      , bazelDeps = deps
      , mavenArtifacts = artifacts
      , mavenRepositories = repos
      }
  where
    firstOf [] = Nothing
    firstOf (x : _) = Just x

    isMavenExt lbl =
      Text.isInfixOf "rules_jvm_external" lbl
        || Text.isInfixOf "maven" lbl

    extractMavenArtifacts :: Map.Map Text StarlarkValue -> (Text, [(Text, StarlarkValue)]) -> [Text]
    extractMavenArtifacts vars (_method, args) =
      case lookup "artifacts" args of
        Just (SList xs) -> xs
        Just (SIdentifier ident) -> case Map.lookup ident vars of
          Just (SList xs) -> xs
          _ -> []
        _ -> []

    extractMavenRepos :: Map.Map Text StarlarkValue -> (Text, [(Text, StarlarkValue)]) -> [Text]
    extractMavenRepos vars (_method, args) =
      case lookup "repositories" args of
        Just (SList xs) -> xs
        Just (SIdentifier ident) -> case Map.lookup ident vars of
          Just (SList xs) -> xs
          _ -> []
        _ -> []

-- | Parse a single top-level statement.
statement :: Parser Statement
statement =
  choice
    [ try moduleStatement
    , try bazelDepStatement
    , try useExtensionStatement
    , try extensionCallStatement
    , try varAssignStatement
    , otherStatement
    ]

-- | Parse: module(name = "...", version = "...")
moduleStatement :: Parser Statement
moduleStatement = do
  _ <- symbol "module"
  args <- parens keywordArgs
  let name = lookupString "name" args
      ver = lookupString "version" args
  case (name, ver) of
    (Just n, Just v) -> pure $ StmtModule (ModuleInfo n v)
    _ -> pure StmtOther

-- | Parse: bazel_dep(name = "...", version = "...", ...)
bazelDepStatement :: Parser Statement
bazelDepStatement = do
  _ <- symbol "bazel_dep"
  args <- parens keywordArgs
  case lookupString "name" args of
    Nothing -> pure StmtOther
    Just name -> do
      let ver = lookupString "version" args
          repo = lookupString "repo_name" args
      pure $
        StmtBazelDep
          BazelDep
            { depName = name
            , depVersion = fromMaybe "" ver
            , depRepoName = repo
            }

-- | Parse: var = use_extension("label", "name")
useExtensionStatement :: Parser Statement
useExtensionStatement = do
  varName <- try $ do
    n <- identifier
    _ <- symbol "="
    _ <- symbol "use_extension"
    pure n
  args <- parens positionalAndKeywordArgs
  case args of
    (lbl : _, _) -> pure $ StmtUseExtension varName (starlarkToText lbl)
    _ -> pure StmtOther

-- | Parse: maven.install(artifacts = [...], ...)
extensionCallStatement :: Parser Statement
extensionCallStatement = do
  (varName, methodName) <- try $ do
    v <- identifier
    _ <- char '.'
    m <- identifier
    pure (v, m)
  args <- parens keywordArgs
  pure $ StmtExtensionCall varName methodName args

-- | Parse: VAR_NAME = value
varAssignStatement :: Parser Statement
varAssignStatement = do
  (name, val) <- try $ do
    n <- identifier
    _ <- symbol "="
    v <- starlarkValue
    pure (n, v)
  pure $ StmtVarAssign name val

-- | Skip an unrecognized top-level statement (function call or other).
otherStatement :: Parser Statement
otherStatement = do
  _ <- identifier <|> (Text.singleton <$> char '@')
  skipMany (try functionCall <|> try dotAccess <|> skipOneToken)
  pure StmtOther
  where
    functionCall = do
      _ <- optional (char '.')
      _ <- optional identifier
      _ <- parens (skipManyTill anySingle (lookAhead (char ')')))
      pure ()
    dotAccess = do
      _ <- char '.'
      _ <- identifier
      pure ()
    skipOneToken = do
      notFollowedBy (char '\n' <|> char '#')
      _ <- anySingle
      pure ()

-- | Parse keyword arguments: name = value, ...
keywordArgs :: Parser [(Text, StarlarkValue)]
keywordArgs = catMaybes <$> arg `sepEndBy` symbol ","
  where
    arg =
      optional $
        try $ do
          name <- identifier
          _ <- symbol "="
          val <- starlarkValue
          pure (name, val)

-- | Parse positional and keyword arguments.
positionalAndKeywordArgs :: Parser ([StarlarkValue], [(Text, StarlarkValue)])
positionalAndKeywordArgs = do
  items <- argItem `sepEndBy` symbol ","
  let positional = [v | Left v <- items]
      keyword = [(k, v) | Right (k, v) <- items]
  pure (positional, keyword)
  where
    argItem =
      try (Right <$> kwArg) <|> (Left <$> starlarkValue)
    kwArg = do
      name <- identifier
      _ <- symbol "="
      val <- starlarkValue
      pure (name, val)

-- | Parse a Starlark value (string, list, bool, integer, identifier).
starlarkValue :: Parser StarlarkValue
starlarkValue =
  choice
    [ SList <$> stringList
    , SString <$> quotedString
    , SBool True <$ symbol "True"
    , SBool False <$ symbol "False"
    , SInt <$> try (lexeme L.decimal)
    , SIdentifier <$> identifier
    ]

-- | Parse a list of strings: ["a", "b", ...]
stringList :: Parser [Text]
stringList = between (symbol "[") (symbol "]") (quotedString `sepEndBy` symbol ",")

-- | Parse a quoted string (single or double quotes).
quotedString :: Parser Text
quotedString = lexeme (doubleQuoted <|> singleQuoted)
  where
    doubleQuoted = between (char '"') (char '"') (Text.pack <$> many (stringChar '"'))
    singleQuoted = between (char '\'') (char '\'') (Text.pack <$> many (stringChar '\''))
    stringChar quoteChar =
      try (char '\\' *> anySingle)
        <|> asum
          [ char c
          | c <- [' ' .. '~']
          , c /= quoteChar
          , c /= '\\'
          ]

-- | Parse an identifier: [a-zA-Z_][a-zA-Z0-9_]*
identifier :: Parser Text
identifier =
  lexeme $
    Text.pack <$> do
      first <- letterChar <|> char '_'
      rest <- many (alphaNumChar <|> char '_')
      pure (first : rest)

-- | Parse content between parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Helper to lookup a string value from keyword args.
lookupString :: Text -> [(Text, StarlarkValue)] -> Maybe Text
lookupString key args = case lookup key args of
  Just (SString s) -> Just s
  _ -> Nothing

-- Helper to convert StarlarkValue to Text.
starlarkToText :: StarlarkValue -> Text
starlarkToText (SString t) = t
starlarkToText (SIdentifier t) = t
starlarkToText _ = ""

-- | Build a dependency graph from parsed MODULE.bazel data.
-- Maven install JSON (if provided) is handled separately.
buildBazelGraph :: BazelModuleFile -> Graphing Dependency
buildBazelGraph moduleFile =
  bazelDepGraph <> mavenArtifactGraph
  where
    bazelDepGraph =
      Graphing.directs (map toBazelDep (bazelDeps moduleFile))

    toBazelDep :: BazelDep -> Dependency
    toBazelDep dep =
      Dependency
        { dependencyType = BazelType
        , dependencyName = depName dep
        , dependencyVersion =
            if Text.null (depVersion dep)
              then Nothing
              else Just (CEq (depVersion dep))
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

    mavenArtifactGraph =
      Graphing.directs (mapMaybe parseMavenCoordinate (mavenArtifacts moduleFile))

    parseMavenCoordinate :: Text -> Maybe Dependency
    parseMavenCoordinate coord =
      case Text.splitOn ":" coord of
        [group, artifact, version] ->
          Just
            Dependency
              { dependencyType = MavenType
              , dependencyName = group <> ":" <> artifact
              , dependencyVersion = Just (CEq version)
              , dependencyLocations = []
              , dependencyEnvironments = mempty
              , dependencyTags = Map.empty
              }
        [group, artifact] ->
          Just
            Dependency
              { dependencyType = MavenType
              , dependencyName = group <> ":" <> artifact
              , dependencyVersion = Nothing
              , dependencyLocations = []
              , dependencyEnvironments = mempty
              , dependencyTags = Map.empty
              }
        _ -> Nothing
