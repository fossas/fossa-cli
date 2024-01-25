module Strategy.Elixir.MixTree (
  PackageName (..),
  MixProject (..),
  MixDep (..),
  MixDepResolved (..),
  DepManager (..),
  DepSCM (..),

  -- * Parsers
  mixTreeCmdOutputParser,
  mixDepsCmdOutputParser,
  parseConstraintExpr,
  toDependencyVersion,

  -- * Graphs and Analyzers
  buildGraph,
  analyze,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, Has, context, fatalText, warn)
import Control.Monad (void, when)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Aeson (ToJSON)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import DepTypes (
  DepType (GitType, HexType),
  Dependency (..),
  VerConstraint (
    CAnd,
    CCompatible,
    CEq,
    CGreater,
    CGreaterOrEq,
    CLess,
    CLessOrEq,
    CNot,
    COr
  ),
 )
import Effect.Exec (AllowErr (Never), Command (..), Exec, execParser)
import GHC.Generics (Generic)
import Graphing (Graphing, unfold)
import Path
import Text.Megaparsec (
  MonadParsec (eof, takeWhile1P, takeWhileP, try),
  Parsec,
  anySingle,
  between,
  choice,
  chunk,
  empty,
  many,
  manyTill,
  optional,
  parse,
  sepBy,
  some,
  takeP,
  (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char, eol, newline, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Types (DependencyResults (..), GraphBreadth (..))

missingDepVersionsMsg :: Text
missingDepVersionsMsg = "Some of dependencies versions were not resolved from `mix deps` and `mix deps.tree`. Has `mix deps.get` and `mix compile` been executed?"

analyze :: (Has Exec sig m, Has Diagnostics sig m) => MixProject -> m DependencyResults
analyze project = do
  let dir = mixDir project
  -- Get all dependencies
  depsAllEnvTree <-
    context "Identifying relationship among dependencies" $
      supportedMixDeps <$> execParser mixTreeCmdOutputParser dir mixDepTreeCmd
  depsAllResolved <-
    context "Inferring dependencies versioning" $
      Map.filter (supportedSCM . depResolvedSCM)
        <$> execParser mixDepsCmdOutputParser dir mixDepCmd

  -- Reminder to get and compile dependencies, if not already done so.
  when (missingResolvedVersions depsAllResolved) $ warn missingDepVersionsMsg
  graph <- context "Building dependency graph" $ pure (buildGraph depsAllEnvTree depsAllResolved)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [mixFile project]
      }

data MixProject = MixProject
  { mixDir :: Path Abs Dir
  , mixFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MixProject

instance AnalyzeProject MixProject where
  analyzeProject _ = analyze
  analyzeProjectStaticOnly _ = const $ fatalText "Cannot analyze mix project statically"

-- | Name of the Package.
newtype PackageName = PackageName {unPackageName :: Text} deriving (Show, Eq, Ord)

-- | Build Manager (:mix, :rebar, :make, :rebar3).
data DepManager = Mix | Rebar3 | Rebar | Make | OtherBuildManager Text deriving (Eq, Show, Ord)

-- | Source Control Manager (SCM) - e.g. git, hex.
data DepSCM = Hex | Git Text (Maybe Text) | Other Text deriving (Eq, Show, Ord)

-- | Mix Dependency (Unresolved - i.e. without versioning).
data MixDep = MixDep
  { depName :: PackageName
  , depVersion :: Maybe VerConstraint
  , depSCM :: DepSCM
  , subDeps :: [MixDep]
  }
  deriving (Eq, Ord, Show)

-- | Mix Dependency (Resolved - i.e. resolved scm, lock ref, and version).
data MixDepResolved = MixDepResolved
  { depResolvedName :: PackageName
  , depResolvedVersion :: Maybe VerConstraint
  , depResolvedSCM :: DepSCM
  , depResolvedRef :: Maybe VerConstraint
  }
  deriving (Show, Ord, Eq)

-- | mix deps --all
mixDepCmd :: Command
mixDepCmd =
  Command
    { cmdName = "mix"
    , cmdArgs = ["deps", "--all"]
    , cmdAllowErr = Never
    }

-- | mix deps.tree --format plain.
mixDepTreeCmd :: Command
mixDepTreeCmd =
  Command
    { cmdName = "mix"
    , cmdArgs = ["deps.tree", "--format", "plain", "--only", "prod"]
    , cmdAllowErr = Never
    }

type Parser = Parsec Void Text

sc :: Parser ()
sc = Lexer.space (void $ some $ char ' ' <|> char '\t') empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: Text -> Parser Text
symbol = Lexer.symbol sc

-- | Parses Dep build manager.
parseDepManager :: Parser DepManager
parseDepManager =
  choice
    [ Mix <$ string "mix"
    , Rebar3 <$ string "rebar3"
    , Rebar <$ string "rebar"
    , Make <$ string "make"
    ]

supportedSCM :: DepSCM -> Bool
supportedSCM Hex = True
supportedSCM (Git _ _) = True
supportedSCM (Other _) = False

-- | Gives only mix dependencies that can be reported.
-- It ignores path dependencies, since they cannot be fetched.
supportedMixDeps :: [MixDep] -> [MixDep]
supportedMixDeps = mapMaybe isSupported
  where
    isSupported m =
      if supportedSCM $ depSCM m
        then
          Just
            MixDep
              { depName = depName m
              , depVersion = depVersion m
              , depSCM = depSCM m
              , subDeps = supportedMixDeps (subDeps m)
              }
        else Nothing

-- | Parses Source Control Manager
parseDepSCM :: Parser DepSCM
parseDepSCM = try parseDepHex <|> parseDepSCMGit <|> parseDepSCMOther
  where
    parseDepHex :: Parser DepSCM
    parseDepHex = Hex <$ chunk "Hex package"

    parseDepSCMGit :: Parser DepSCM
    parseDepSCMGit = do
      uriScheme <- chunk "http://" <|> chunk "https://" <|> chunk "git://"
      uriRest <- takeWhileP (Just "git address") (\c -> c /= ' ' && c /= ')')

      reference <- optional . try $ do
        _ <- chunk " - "
        toText <$> some (alphaNumChar <|> char '.')

      pure $ Git (uriScheme <> uriRest) reference

    parseDepSCMOther :: Parser DepSCM
    parseDepSCMOther = Other <$> takeWhileP (Just "OtherSCM") (/= ')')

-- | True if a version is not resolved in `MixDepResolved`, otherwise False.
-- This can happen, if dependencies are not retrieved or compiled.
missingResolvedVersions :: Map PackageName MixDepResolved -> Bool
missingResolvedVersions mdr = any (isNothing . depResolvedVersion . snd) (Map.toList mdr)

-- Parses `mix deps` output
mixDepsCmdOutputParser :: Parser (Map PackageName MixDepResolved)
mixDepsCmdOutputParser = Map.fromList <$> (parseDep `sepBy` char '*') <* eof
  where
    findName :: Parser Text
    findName = takeWhileP (Just "dep") (/= ' ')

    findVersion :: Parser Text
    findVersion = toText <$> some (alphaNumChar <|> char '.' <|> char '-')

    parseDep :: Parser (PackageName, MixDepResolved)
    parseDep = do
      _ <- lexeme (chunk "*" <|> chunk " ")

      -- Package name
      name <- lexeme findName

      -- If dependencies are not resolved, version won't be printed
      -- This can occur if lock file is missing
      version <- lexeme $ optional . try $ CEq <$> findVersion

      -- Source control manager of the package - e.g. git, hex
      scm <- lexeme $ between (symbol "(") (symbol ")") parseDepSCM

      -- If dependencies are not resolved, build manager won't be printed
      -- This can occur if lock file is missing.
      _ <- lexeme $ optional . try $ between (symbol "(") (symbol ")") parseDepManager
      _ <- lexeme newline

      -- If dependencies are not resolved, locked reference won't be printed
      ref <- lexeme $ optional . try $ CEq <$> (lexeme (chunk "locked at") *> findVersion)

      -- Ignore status line, and rest until next entry
      _ <- takeWhileP (Just "ignored") (/= '*')
      pure (PackageName name, MixDepResolved (PackageName name) version scm ref)

-- | Parses mix deps.tree raw output
mixTreeCmdOutputParser :: Parser [MixDep]
mixTreeCmdOutputParser = manyTill anySingle newline *> mixTreeParser

-- | Parses dependency tree generated by mix deps.tree
mixTreeParser :: Parser [MixDep]
mixTreeParser = concat <$> ((try (mixDep 0) <|> ignoredLine) `sepBy` eol) <* eof
  where
    isEndLine :: Char -> Bool
    isEndLine '\n' = True
    isEndLine '\r' = True
    isEndLine _ = False

    ignoredLine :: Parser [MixDep]
    ignoredLine = ignored $> []

    ignored :: Parser ()
    ignored = void $ takeWhileP (Just "ignored") (not . isEndLine)

    findName :: Parser Text
    findName = takeWhileP (Just "dependency name") (/= ' ')

    findRequirement :: Parser Text
    findRequirement = takeWhile1P (Just "requirements") (/= '(')

    mixDep :: Int -> Parser [MixDep]
    mixDep depth = do
      _ <- takeP (Just "spacing ~ one of (`), (|), ( )") (1 + depth * 4)

      -- Dependency's name
      dep <- lexeme (chunk "--") *> (lexeme findName)

      -- Dependency's version
      version <- optional . try $ findRequirement

      -- Dependency's scm
      scm <- between (char '(') (char ')') parseDepSCM
      _ <- takeWhileP (Just "ignored") (not . isEndLine)

      -- Deep Dependencies
      deps <- many $ try $ mixRecurse (depth + 1)
      pure [MixDep (PackageName dep) (version >>= toDependencyVersion) scm (concat deps)]

    mixRecurse :: Int -> Parser [MixDep]
    mixRecurse depth = chunk "\n" *> mixDep depth

buildGraph :: [MixDep] -> (Map PackageName MixDepResolved) -> Graphing Dependency
buildGraph deps depsResolved = unfold deps subDeps toDependency
  where
    toDependency md =
      Dependency
        { dependencyType = dType md
        , dependencyName = dName md depsResolved
        , dependencyVersion = dVersion md depsResolved
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

    dType :: MixDep -> DepType
    dType m = case (depSCM m) of
      Git _ _ -> GitType
      _ -> HexType

    dName :: MixDep -> Map PackageName MixDepResolved -> Text
    dName m dr = fromMaybe (reqsName) (resolvedGitUrl)
      where
        resolvedGitUrl :: Maybe Text
        resolvedGitUrl = case depResolvedSCM <$> Map.lookup (depName m) dr of
          Just (Git url _) -> Just url
          _ -> Nothing

        reqsName :: Text
        reqsName = case depSCM m of
          Git url _ -> url
          _ -> unPackageName $ depName m

    -- Favors in order of locked reference, resolved version, and requirement constraint
    dVersion :: MixDep -> Map PackageName MixDepResolved -> Maybe VerConstraint
    dVersion m dr =
      asum
        [ Map.lookup (depName m) dr >>= depResolvedVersion
        , Map.lookup (depName m) dr >>= depResolvedRef
        , rawVersion
        ]
      where
        rawVersion = case (depSCM m) of
          Git _ (Just ref) -> Just $ CEq ref
          _ -> depVersion m

toDependencyVersion :: Text -> Maybe VerConstraint
toDependencyVersion dt = case parse parseConstraintExpr "" dt of
  Left _ -> Nothing
  Right (CEq "*") -> Nothing
  Right vc -> Just vc

-- | Parses `VerConstraint`
parseVerConstraint :: Parser VerConstraint
parseVerConstraint = do
  operator <- whitespaceOrTab *> parseConstraintOperator <* whitespaceOrTab
  versionText <- findVersionText <* whitespaceOrTab
  case operator of
    Just "==" -> pure $ CEq versionText
    Just "=" -> pure $ CEq versionText
    Just "!=" -> pure $ CNot versionText
    Just ">=" -> pure $ CGreaterOrEq versionText
    Just ">" -> pure $ CGreater versionText
    Just "<=" -> pure $ CLessOrEq versionText
    Just "<" -> pure $ CLess versionText
    Just "~>" -> pure $ CCompatible versionText
    Just "*" -> pure $ CEq "*"
    Just _ -> fail ("Could not recognize mix constraint operator:")
    Nothing -> pure $ CEq versionText
  where
    whitespaceOrTab :: Parser Text
    whitespaceOrTab = takeWhileP (Just "whitespaceOrTab") (\c -> c == ' ' || c == '\t')

    parseConstraintOperator :: Parser (Maybe Text)
    parseConstraintOperator = optional . asum $ map symbol operatorList
      where
        operatorList = [">=", "<=", ">", "<", "==", "!=", "~>", "="] :: [Text]

    findVersionText :: Parser Text
    findVersionText = toText <$> some (alphaNumChar <|> char '.' <|> char '-' <|> char '*' <|> char '+')

-- | Parses [mix constraint expression](https://hexdocs.pm/elixir/1.12/Version.html).
parseConstraintExpr :: Parser VerConstraint
parseConstraintExpr = makeExprParser parseVerConstraint operatorTable
  where
    operatorTable :: [[Operator Parser VerConstraint]]
    operatorTable =
      [ [binary "or" COr]
      , [binary "and" CAnd]
      ]
      where
        binary :: Text -> (VerConstraint -> VerConstraint -> VerConstraint) -> Operator Parser VerConstraint
        binary name f = InfixL (f <$ symbol name)
