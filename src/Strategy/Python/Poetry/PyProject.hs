module Strategy.Python.Poetry.PyProject (
  PyProject (..),
  PyProjectMetadata (..),
  PyProjectTool (..),
  PyProjectPdm (..),
  PyProjectPoetryGroup (..),
  PyProjectPoetryGroupDependencies (..),
  PyProjectPoetry (..),
  PyProjectBuildSystem (..),
  PoetryDependency (..),
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
import Strategy.Python.Util (Req)
import Text.Megaparsec (
  Parsec,
  empty,
  many,
  noneOf,
  optional,
  parse,
  some,
  takeWhileP,
 )
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Toml qualified
import Toml.Schema qualified

type Parser = Parsec Void Text

newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Ord, Show)

-- | Represents Pyproject.
--
-- Both `pyprojectBuildSystem` and `pyprojectPoetry` can be `Nothing`,
-- when pyproject does not use poetry for build or has no dependencies.
data PyProject = PyProject
  { pyprojectBuildSystem :: Maybe PyProjectBuildSystem
  , pyprojectProject :: Maybe PyProjectMetadata
  , pyprojectTool :: Maybe PyProjectTool
  }
  deriving (Show, Eq, Ord)

instance Toml.Schema.FromValue PyProject where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProject
        <$> Toml.Schema.optKey "build-system"
        <*> Toml.Schema.optKey "project"
        <*> Toml.Schema.optKey "tool"

data PyProjectTool = PyProjectTool
  { pyprojectPoetry :: Maybe PyProjectPoetry
  , pyprojectPdm :: Maybe PyProjectPdm
  }
  deriving (Show, Eq, Ord)

instance Toml.Schema.FromValue PyProjectTool where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectTool
        <$> Toml.Schema.optKey "poetry"
        <*> Toml.Schema.optKey "pdm"

newtype PyProjectPdm = PyProjectPdm
  {pdmDevDependencies :: Maybe (Map Text [Req])}
  deriving (Show, Eq, Ord)

instance Toml.Schema.FromValue PyProjectPdm where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectPdm
        <$> Toml.Schema.optKey "dev-dependencies"

instance Toml.Schema.FromValue PyProjectMetadata where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectMetadata
        <$> Toml.Schema.optKey "dependencies"
        <*> Toml.Schema.optKey "optional-dependencies"

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

newtype PyProjectBuildSystem = PyProjectBuildSystem
  { buildBackend :: Text
  }
  deriving (Show, Eq, Ord)

instance Toml.Schema.FromValue PyProjectBuildSystem where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectBuildSystem
        <$> Toml.Schema.reqKey "build-backend"

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
    pyprojectPoetryGroup :: Maybe PyProjectPoetryGroup
  }
  deriving (Show, Eq, Ord)

instance Toml.Schema.FromValue PyProjectPoetry where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectPoetry
        <$> Toml.Schema.optKey "name"
        <*> Toml.Schema.optKey "version"
        <*> Toml.Schema.optKey "description"
        <*> Toml.Schema.pickKey [Toml.Schema.Key "dependencies" Toml.Schema.fromValue, Toml.Schema.Else (pure mempty)]
        <*> Toml.Schema.pickKey [Toml.Schema.Key "dev-dependencies" Toml.Schema.fromValue, Toml.Schema.Else (pure mempty)]
        <*> Toml.Schema.optKey "group"

data PyProjectPoetryGroup = PyProjectPoetryGroup
  { groupDev :: Maybe PyProjectPoetryGroupDependencies
  , groupTest :: Maybe PyProjectPoetryGroupDependencies
  }
  deriving (Show, Eq, Ord)

instance Toml.Schema.FromValue PyProjectPoetryGroup where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectPoetryGroup
        <$> Toml.Schema.optKey "dev"
        <*> Toml.Schema.optKey "test"

newtype PyProjectPoetryGroupDependencies = PyProjectPoetryGroupDependencies
  {groupDependencies :: Map Text PoetryDependency}
  deriving (Show, Eq, Ord)

instance Toml.Schema.FromValue PyProjectPoetryGroupDependencies where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectPoetryGroupDependencies
        <$> Toml.Schema.pickKey [Toml.Schema.Key "dependencies" Toml.Schema.fromValue, Toml.Schema.Else (pure mempty)]

allPoetryProductionDeps :: PyProject -> Map Text PoetryDependency
allPoetryProductionDeps project = case pyprojectTool project of
  Just (PyProjectTool{pyprojectPoetry}) -> case pyprojectPoetry of
    Just (PyProjectPoetry{dependencies}) -> dependencies
    _ -> mempty
  _ -> mempty

allPoetryNonProductionDeps :: PyProject -> Map Text PoetryDependency
allPoetryNonProductionDeps project = unions [olderPoetryDevDeps, optionalDeps]
  where
    optionalDeps :: Map Text PoetryDependency
    optionalDeps = case pyprojectTool project of
      Just (PyProjectTool{pyprojectPoetry}) -> case pyprojectPoetry of
        Just (PyProjectPoetry{pyprojectPoetryGroup}) -> case pyprojectPoetryGroup of
          Just (PyProjectPoetryGroup{groupDev, groupTest}) -> case (groupDev, groupTest) of
            (Just devDeps, Just testDeps) -> unions [groupDependencies devDeps, groupDependencies testDeps]
            (Just devDeps, Nothing) -> groupDependencies devDeps
            (Nothing, Just testDeps) -> groupDependencies testDeps
            _ -> mempty
          _ -> mempty
        _ -> mempty
      _ -> mempty

    olderPoetryDevDeps :: Map Text PoetryDependency
    olderPoetryDevDeps = case pyprojectTool project of
      Just (PyProjectTool{pyprojectPoetry}) -> case pyprojectPoetry of
        Just (PyProjectPoetry{devDependencies}) -> devDependencies
        _ -> mempty
      _ -> mempty

data PoetryDependency
  = PoetryTextVersion Text
  | PyProjectPoetryDetailedVersionDependencySpec PyProjectPoetryDetailedVersionDependency
  | PyProjectPoetryGitDependencySpec PyProjectPoetryGitDependency
  | PyProjectPoetryPathDependencySpec PyProjectPoetryPathDependency
  | PyProjectPoetryUrlDependencySpec PyProjectPoetryUrlDependency
  deriving (Show, Eq, Ord)

instance Toml.Schema.FromValue PoetryDependency where
  fromValue (Toml.Text' _ t) = pure $ PoetryTextVersion t
  fromValue v@(Toml.Table' l t) =
    Toml.Schema.parseTable
      ( Toml.Schema.pickKey $
          [Toml.Schema.Key "version" (const (PyProjectPoetryDetailedVersionDependencySpec <$> Toml.Schema.fromValue v))]
            <> [Toml.Schema.Key "git" (const (PyProjectPoetryGitDependencySpec <$> Toml.Schema.fromValue v))]
            <> [Toml.Schema.Key "path" (const (PyProjectPoetryPathDependencySpec <$> Toml.Schema.fromValue v))]
            <> [Toml.Schema.Key "url" (const (PyProjectPoetryUrlDependencySpec <$> Toml.Schema.fromValue v))]
            <> [Toml.Schema.Else (Toml.Schema.failAt (Toml.valueAnn v) "invalid spec")]
      )
      l
      t
  fromValue v = Toml.Schema.failAt (Toml.valueAnn v) $ "invalid poetry dependency" <> Toml.valueType v

newtype PyProjectPoetryDetailedVersionDependency = PyProjectPoetryDetailedVersionDependency
  { poetryDependencyVersion :: Text
  }
  deriving (Show, Eq, Ord)

instance Toml.Schema.FromValue PyProjectPoetryDetailedVersionDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectPoetryDetailedVersionDependency
        <$> Toml.Schema.reqKey "version"

data PyProjectPoetryGitDependency = PyProjectPoetryGitDependency
  { gitUrl :: Text
  , gitBranch :: Maybe Text
  , gitRev :: Maybe Text
  , gitTag :: Maybe Text
  }
  deriving (Show, Eq, Ord)

instance Toml.Schema.FromValue PyProjectPoetryGitDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectPoetryGitDependency
        <$> Toml.Schema.reqKey "git"
        <*> Toml.Schema.optKey "branch"
        <*> Toml.Schema.optKey "rev"
        <*> Toml.Schema.optKey "tag"

newtype PyProjectPoetryPathDependency = PyProjectPoetryPathDependency
  { sourcePath :: Text
  }
  deriving (Show, Eq, Ord)

instance Toml.Schema.FromValue PyProjectPoetryPathDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectPoetryPathDependency
        <$> Toml.Schema.reqKey "path"

newtype PyProjectPoetryUrlDependency = PyProjectPoetryUrlDependency
  { sourceUrl :: Text
  }
  deriving (Show, Eq, Ord)

instance Toml.Schema.FromValue PyProjectPoetryUrlDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectPoetryUrlDependency
        <$> Toml.Schema.reqKey "url"

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
