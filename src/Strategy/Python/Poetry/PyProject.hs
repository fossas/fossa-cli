module Strategy.Python.Poetry.PyProject (
  PyProject (..),
  PyProjectMetadata (..),
  PyProjectPoetry (..),
  PyProjectBuildSystem (..),
  PoetryDependency (..),
  pyProjectCodec,
  PyProjectPoetryPathDependency (..),
  PyProjectPoetryGitDependency (..),
  PyProjectPoetryUrlDependency (..),
  PyProjectPoetryDetailedVersionDependency (..),
  allPoetryProductionDeps,

  -- * for testing only
  parseConstraintExpr,
  toDependencyVersion,
  allPoetryNonProductionDeps,
) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Foldable (asum)
import Data.Functor (void)
import Data.Map (Map, unions)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Void (Void)
import DepTypes (
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
import Strategy.Python.Util (Req, reqCodec)
import Text.Megaparsec (
  Parsec,
  empty,
  many,
  noneOf,
  optional,
  parse,
  some,
  takeWhileP,
  (<|>),
 )
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Toml (TomlCodec, (.=))
import Toml qualified

type Parser = Parsec Void Text

newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Ord, Show)

-- | Represents Pyproject.
--
-- Both `pyprojectBuildSystem` and `pyprojectPoetry` can be `Nothing`,
-- when pyproject does not use poetry for build or has no dependencies.
data PyProject = PyProject
  { pyprojectBuildSystem :: Maybe PyProjectBuildSystem
  , pyprojectPoetry :: Maybe PyProjectPoetry
  , pyprojectProject :: Maybe PyProjectMetadata
  , pyprojectPdmDevDependencies :: Maybe (Map Text [Req])
  }
  deriving (Show, Eq, Ord)

pyProjectCodec :: TomlCodec PyProject
pyProjectCodec =
  PyProject
    <$> Toml.dioptional (Toml.table pyProjectBuildSystemCodec "build-system") .= pyprojectBuildSystem
    <*> Toml.dioptional (Toml.table pyProjectPoetryCodec "tool.poetry") .= pyprojectPoetry
    <*> Toml.dioptional (Toml.table pyPyProjectMetadataCodec "project") .= pyprojectProject
    <*> Toml.dioptional (Toml.tableMap Toml._KeyText (Toml.arrayOf reqCodec) "tool.pdm.dev-dependencies") .= pyprojectPdmDevDependencies

-- | Represents [project] block
-- > [project]
-- > dependencies = [...]
-- >
-- > [project.optional-dependencies]
-- > gui = ["PyQt5"]
-- > cli = ["click"]
-- >
-- Refer to: https://packaging.python.org/en/latest/specifications/declaring-project-metadata/#dependencies-optional-dependencies
data PyProjectMetadata = PyProjectMetadata
  { pyprojectDependencies :: Maybe [Req]
  , pyprojectOptionalDependencies :: Maybe (Map Text [Req])
  }
  deriving (Show, Eq, Ord)

pyPyProjectMetadataCodec :: TomlCodec PyProjectMetadata
pyPyProjectMetadataCodec =
  PyProjectMetadata
    <$> Toml.dioptional (Toml.arrayOf reqCodec "dependencies") .= pyprojectDependencies
    <*> Toml.dioptional (Toml.tableMap Toml._KeyText (Toml.arrayOf reqCodec) "optional-dependencies") .= pyprojectOptionalDependencies

newtype PyProjectBuildSystem = PyProjectBuildSystem
  { buildBackend :: Text
  }
  deriving (Show, Eq, Ord)

pyProjectBuildSystemCodec :: TomlCodec PyProjectBuildSystem
pyProjectBuildSystemCodec =
  PyProjectBuildSystem
    <$> Toml.diwrap (Toml.text "build-backend") .= buildBackend

data PyProjectPoetry = PyProjectPoetry
  { name :: Maybe Text
  , version :: Maybe Text
  , description :: Maybe Text
  , dependencies :: Map Text PoetryDependency
  , -- For Poetry pre-1.2.x style, understood by Poetry 1.0–1.2
    devDependencies :: Map Text PoetryDependency
  , -- Since v1.2.0 of poetry, dependency groups are recommanded way to
    -- provide development, test, and other optional dependencies.
    -- refer to: https://python-poetry.org/docs/managing-dependencies#dependency-groups
    --
    -- Due to current toml-parsing limitations, we explicitly specify dev, and
    -- test group only. Note that any dependencies from these groups are excluded
    -- by default. refer to: https://github.com/kowainik/tomland/issues/336
    groupDevDependencies :: Map Text PoetryDependency
  , groupTestDependencies :: Map Text PoetryDependency
  }
  deriving (Show, Eq, Ord)

allPoetryProductionDeps :: PyProject -> Map Text PoetryDependency
allPoetryProductionDeps project = case pyprojectPoetry project of
  Just (PyProjectPoetry{dependencies}) -> dependencies
  _ -> mempty

allPoetryNonProductionDeps :: PyProject -> Map Text PoetryDependency
allPoetryNonProductionDeps project = unions [olderPoetryDevDeps, optionalDeps]
  where
    optionalDeps :: Map Text PoetryDependency
    optionalDeps = case pyprojectPoetry project of
      Just (PyProjectPoetry{groupDevDependencies, groupTestDependencies}) -> unions [groupDevDependencies, groupTestDependencies]
      _ -> mempty

    olderPoetryDevDeps :: Map Text PoetryDependency
    olderPoetryDevDeps = case pyprojectPoetry project of
      Just (PyProjectPoetry{devDependencies}) -> devDependencies
      _ -> mempty

data PoetryDependency
  = PoetryTextVersion Text
  | PyProjectPoetryDetailedVersionDependencySpec PyProjectPoetryDetailedVersionDependency
  | PyProjectPoetryGitDependencySpec PyProjectPoetryGitDependency
  | PyProjectPoetryPathDependencySpec PyProjectPoetryPathDependency
  | PyProjectPoetryUrlDependencySpec PyProjectPoetryUrlDependency
  deriving (Show, Eq, Ord)

pyProjectPoetryCodec :: TomlCodec PyProjectPoetry
pyProjectPoetryCodec =
  PyProjectPoetry
    <$> Toml.dioptional (Toml.text "name") .= name
    <*> Toml.dioptional (Toml.text "version") .= version
    <*> Toml.dioptional (Toml.text "description") .= description
    <*> Toml.tableMap Toml._KeyText pyProjectPoetryDependencyCodec "dependencies" .= dependencies
    <*> Toml.tableMap Toml._KeyText pyProjectPoetryDependencyCodec "dev-dependencies" .= devDependencies
    <*> Toml.tableMap Toml._KeyText pyProjectPoetryDependencyCodec "group.dev.dependencies" .= groupDevDependencies
    <*> Toml.tableMap Toml._KeyText pyProjectPoetryDependencyCodec "group.test.dependencies" .= groupTestDependencies

pyProjectPoetryDependencyCodec :: Toml.Key -> TomlCodec PoetryDependency
pyProjectPoetryDependencyCodec key =
  Toml.dimatch matchPyProjectPoetryTextVersionDependecySpec PoetryTextVersion (Toml.text key)
    <|> Toml.dimatch matchPyProjectPoetryDetailedVersionDependencySpec PyProjectPoetryDetailedVersionDependencySpec (Toml.table pyProjectPoetryDetailedVersionDependencyCodec key)
    <|> Toml.dimatch matchPyProjectPoetryGitDependencySpec PyProjectPoetryGitDependencySpec (Toml.table pyProjectPoetryGitDependencyCodec key)
    <|> Toml.dimatch matchPyProjectPoetryPathDependencySpec PyProjectPoetryPathDependencySpec (Toml.table pyProjectPoetryPathDependencyCodec key)
    <|> Toml.dimatch matchPyProjectPoetryUrlDependencySpec PyProjectPoetryUrlDependencySpec (Toml.table pyProjectPoetryUrlDependencyCodec key)

matchPyProjectPoetryTextVersionDependecySpec :: PoetryDependency -> Maybe Text
matchPyProjectPoetryTextVersionDependecySpec (PoetryTextVersion version) = Just version
matchPyProjectPoetryTextVersionDependecySpec _ = Nothing

matchPyProjectPoetryDetailedVersionDependencySpec :: PoetryDependency -> Maybe PyProjectPoetryDetailedVersionDependency
matchPyProjectPoetryDetailedVersionDependencySpec (PyProjectPoetryDetailedVersionDependencySpec spec) = Just spec
matchPyProjectPoetryDetailedVersionDependencySpec _ = Nothing

matchPyProjectPoetryGitDependencySpec :: PoetryDependency -> Maybe PyProjectPoetryGitDependency
matchPyProjectPoetryGitDependencySpec (PyProjectPoetryGitDependencySpec spec) = Just spec
matchPyProjectPoetryGitDependencySpec _ = Nothing

matchPyProjectPoetryPathDependencySpec :: PoetryDependency -> Maybe PyProjectPoetryPathDependency
matchPyProjectPoetryPathDependencySpec (PyProjectPoetryPathDependencySpec spec) = Just spec
matchPyProjectPoetryPathDependencySpec _ = Nothing

matchPyProjectPoetryUrlDependencySpec :: PoetryDependency -> Maybe PyProjectPoetryUrlDependency
matchPyProjectPoetryUrlDependencySpec (PyProjectPoetryUrlDependencySpec spec) = Just spec
matchPyProjectPoetryUrlDependencySpec _ = Nothing

newtype PyProjectPoetryDetailedVersionDependency = PyProjectPoetryDetailedVersionDependency
  { poetryDependencyVersion :: Text
  }
  deriving (Show, Eq, Ord)

pyProjectPoetryDetailedVersionDependencyCodec :: TomlCodec PyProjectPoetryDetailedVersionDependency
pyProjectPoetryDetailedVersionDependencyCodec =
  PyProjectPoetryDetailedVersionDependency
    <$> Toml.text "version" .= poetryDependencyVersion

data PyProjectPoetryGitDependency = PyProjectPoetryGitDependency
  { gitUrl :: Text
  , gitBranch :: Maybe Text
  , gitRev :: Maybe Text
  , gitTag :: Maybe Text
  }
  deriving (Show, Eq, Ord)

pyProjectPoetryGitDependencyCodec :: TomlCodec PyProjectPoetryGitDependency
pyProjectPoetryGitDependencyCodec =
  PyProjectPoetryGitDependency
    <$> Toml.text "git" .= gitUrl
    <*> Toml.dioptional (Toml.text "branch") .= gitBranch
    <*> Toml.dioptional (Toml.text "rev") .= gitRev
    <*> Toml.dioptional (Toml.text "tag") .= gitTag

newtype PyProjectPoetryPathDependency = PyProjectPoetryPathDependency
  { sourcePath :: Text
  }
  deriving (Show, Eq, Ord)

pyProjectPoetryPathDependencyCodec :: TomlCodec PyProjectPoetryPathDependency
pyProjectPoetryPathDependencyCodec =
  PyProjectPoetryPathDependency
    <$> Toml.text "path" .= sourcePath

newtype PyProjectPoetryUrlDependency = PyProjectPoetryUrlDependency
  { sourceUrl :: Text
  }
  deriving (Show, Eq, Ord)

pyProjectPoetryUrlDependencyCodec :: TomlCodec PyProjectPoetryUrlDependency
pyProjectPoetryUrlDependencyCodec =
  PyProjectPoetryUrlDependency
    <$> Toml.text "url" .= sourceUrl

toDependencyVersion :: Text -> Maybe VerConstraint
toDependencyVersion dt = case parse parseConstraintExpr "" dt of
  Left _ -> Nothing
  Right (CEq "*") -> Nothing
  Right v -> Just v

symbol :: Text -> Parser Text
symbol = Lexer.symbol sc
  where
    sc :: Parser ()
    sc = Lexer.space (void $ some $ char ' ') empty empty

-- | Consumes whitespace ` ` or tab `\t`
whitespaceOrTab :: Parser Text
whitespaceOrTab = takeWhileP (Just "whitespaceOrTab") isSpaceOrTab
  where
    isSpaceOrTab c = c == ' ' || c == '\t'

-- | Poetry constraint operators
data PoetryConstraintOperators
  = GreaterThanOrEqual
  | GreaterThan
  | LessThanOrEqual
  | LessThan
  | Equal
  | NotEqual
  | WildcardAny
  | MajorCompatible
  | MinorCompatible

-- | Parses `VerConstraint`.
parseVerConstraint :: Parser VerConstraint
parseVerConstraint = do
  operator <- whitespaceOrTab *> parseConstraintOperator <* whitespaceOrTab
  versionText <- many (noneOf [andOperatorChar, orOperatorChar] <* whitespaceOrTab)
  let v = toText versionText
  case operator of
    Equal -> pure $ CEq v
    NotEqual -> pure $ CNot v
    GreaterThanOrEqual -> pure $ CGreaterOrEq v
    GreaterThan -> pure $ CGreater v
    LessThanOrEqual -> pure $ CLessOrEq v
    LessThan -> pure $ CLess v
    MajorCompatible -> pure $ CCompatible v
    MinorCompatible -> pure $ CCompatible v
    WildcardAny -> pure $ CEq "*"
  where
    andOperatorChar = ','
    orOperatorChar = '|'

-- | Parses poetry constraint operator.
parseConstraintOperator :: Parser PoetryConstraintOperators
parseConstraintOperator = fromMaybe Equal <$> optional (asum (map symbol operatorList) >>= textToPoetryVersion)
  where
    operatorList = [">=", "<=", "~=", ">", "<", "^", "==", "===", "!=", "~", "*", "="] :: [Text]

    textToPoetryVersion :: (MonadFail m) => Text -> m PoetryConstraintOperators
    textToPoetryVersion = \case
      "===" -> pure Equal
      "==" -> pure Equal
      "!=" -> pure NotEqual
      ">=" -> pure GreaterThanOrEqual
      "<=" -> pure LessThanOrEqual
      "~=" -> pure MinorCompatible
      "=" -> pure Equal
      ">" -> pure GreaterThan
      "<" -> pure LessThan
      "^" -> pure MajorCompatible
      "~" -> pure MinorCompatible
      "*" -> pure WildcardAny
      other -> fail ("Could not recognize poetry constraint operator: " <> toString other)

-- | Parses [poetry constraint expression](https://python-poetry.org/docs/dependency-specification/)
-- found in Pyproject.toml into `VerConstraint`.
parseConstraintExpr :: Parser VerConstraint
parseConstraintExpr = makeExprParser parseVerConstraint operatorTable
  where
    operatorTable :: [[Operator Parser VerConstraint]]
    operatorTable =
      [ [binary "||" COr]
      , [binary "," CAnd]
      ]
      where
        binary :: Text -> (VerConstraint -> VerConstraint -> VerConstraint) -> Operator Parser VerConstraint
        binary name f = InfixL (f <$ symbol name)
