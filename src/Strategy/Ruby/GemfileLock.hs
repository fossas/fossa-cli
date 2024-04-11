module Strategy.Ruby.GemfileLock (
  analyze',
  findSections,
  buildGraph,
  Spec (..),
  SpecDep (..),
  DirectDep (..),
  Section (..),
) where

import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Char qualified as C
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe ()
import Data.Set (Set)
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Void (Void)
import DepTypes
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Path
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Remote = Text
type Revision = Text
type Branch = Text

data Section
  = GitSection Remote (Maybe Revision) (Maybe Branch) [Spec]
  | GemSection Remote [Spec]
  | PathSection Remote [Spec]
  | DependencySection [DirectDep]
  | UnknownSection Text
  deriving (Eq, Ord, Show)

data Spec = Spec
  { specVersion :: Text
  , specName :: Text
  , specDeps :: [SpecDep]
  }
  deriving (Eq, Ord, Show)

newtype SpecDep = SpecDep
  { depName :: Text
  }
  deriving (Eq, Ord, Show)

newtype DirectDep = DirectDep
  { directName :: Text
  }
  deriving (Eq, Ord, Show)

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = do
  sections <- readContentsParser @[Section] findSections file
  context "Building dependency graph" $ pure (buildGraph sections)

newtype GemfilePkg = GemfilePkg {pkgName :: Text}
  deriving (Eq, Ord, Show)

type GemfileGrapher = LabeledGrapher GemfilePkg GemfileLabel

data GemfileLabel
  = GemfileVersion Text
  | -- | repo url, revision
    GitRemote Text (Maybe Text)
  | -- | url
    OtherRemote Text
  deriving (Eq, Ord, Show)

toDependency :: GemfilePkg -> Set GemfileLabel -> Dependency
toDependency pkg = foldr applyLabel start
  where
    start :: Dependency
    start =
      Dependency
        { dependencyType = GemType
        , dependencyName = pkgName pkg
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

    applyLabel :: GemfileLabel -> Dependency -> Dependency
    applyLabel (GemfileVersion ver) dep = dep{dependencyVersion = (dependencyVersion dep) <|> (Just $ CEq ver)}
    applyLabel (GitRemote repo maybeRevision) dep =
      dep{dependencyType = GitType, dependencyName = repo, dependencyVersion = maybe Nothing (\revision -> Just (CEq revision)) maybeRevision, dependencyLocations = maybe repo (\revision -> repo <> "@" <> revision) maybeRevision : dependencyLocations dep}
    applyLabel (OtherRemote loc) dep =
      dep{dependencyLocations = loc : dependencyLocations dep}

buildGraph :: [Section] -> Graphing Dependency
buildGraph sections =
  run . withLabeling toDependency $
    traverse_ addSection sections
  where
    addSection :: Has GemfileGrapher sig m => Section -> m ()
    addSection (DependencySection deps) = traverse_ (direct . GemfilePkg . directName) deps
    addSection (GitSection remote revision branch specs) =
      traverse_ (addSpec (GitRemote remote (revision <|> branch))) specs
    addSection (PathSection remote specs) =
      traverse_ (addSpec (OtherRemote remote)) specs
    addSection (GemSection remote specs) =
      traverse_ (addSpec (OtherRemote remote)) specs
    addSection UnknownSection{} = pure ()

    addSpec :: Has GemfileGrapher sig m => GemfileLabel -> Spec -> m ()
    addSpec remoteLabel spec = do
      let pkg = GemfilePkg (specName spec)
      -- add edges between spec and specdeps
      traverse_ (edge pkg . GemfilePkg . depName) (specDeps spec)
      -- add a label for version
      label pkg (GemfileVersion (specVersion spec))
      -- add a label for remote
      label pkg remoteLabel

type Parser = Parsec Void Text

findSections :: Parser [Section]
findSections = manyTill (try gitSectionParser <|> try gemSectionParser <|> try pathSectionParser <|> try dependenciesSectionParser <|> unknownSection) eof

unknownSection :: Parser Section
unknownSection = UnknownSection <$ scn <*> restOfLine

restOfLine :: Parser Text
restOfLine = takeWhileP (Just "ignored") (not . isEndLine)

-- ignore content until the end of the line
ignored :: Parser ()
ignored = void $ takeWhileP (Just "ignored") (not . isEndLine)

gitSectionParser :: Parser Section
gitSectionParser = mkSectionParser "GIT" $ \propertyMap -> do
  remote <- lookupRawText "remote" propertyMap
  specs <- lookupRawSpecs "specs" propertyMap
  let revision = eitherToMaybe $ lookupRawText "revision" propertyMap
      branch = eitherToMaybe $ lookupRawText "branch" propertyMap
  pure $ GitSection remote revision branch specs

pathSectionParser :: Parser Section
pathSectionParser = mkSectionParser "PATH" $ \propertyMap -> do
  remote <- lookupRawText "remote" propertyMap
  specs <- lookupRawSpecs "specs" propertyMap
  pure $ PathSection remote specs

gemSectionParser :: Parser Section
gemSectionParser = mkSectionParser "GEM" $ \propertyMap -> do
  remote <- lookupRawText "remote" propertyMap
  specs <- lookupRawSpecs "specs" propertyMap
  pure $ GemSection remote specs

mkSectionParser :: Text -> (Map Text RawField -> Either Text Section) -> Parser Section
mkSectionParser sectionName toSection = L.nonIndented scn $
  L.indentBlock scn $ do
    _ <- chunk sectionName
    pure $ L.IndentMany Nothing propertiesToSection (try propertyParser <|> specPropertyParser)
  where
    propertiesToSection :: [(Text, RawField)] -> Parser Section
    propertiesToSection properties =
      let propertyMap = Map.fromList properties
          result :: Either Text Section
          result = toSection propertyMap
       in case result of
            Right x -> pure x
            Left y -> fail $ toString $ "could not parse " <> sectionName <> " section: " <> y

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right a) = Just a
eitherToMaybe (Left _) = Nothing

lookupRawText :: Text -> Map Text RawField -> Either Text Text
lookupRawText key m = case Map.lookup key m of
  Just (RawText val) -> Right val
  _ -> Left $ "a value for " <> key <> " was unable to be found in the map"

lookupRawSpecs :: Text -> Map Text RawField -> Either Text [Spec]
lookupRawSpecs key m = case Map.lookup key m of
  Just (RawSpecs val) -> Right val
  _ -> Left $ "a value for " <> key <> " was unable to be found in the map"

data RawField
  = RawText Text
  | RawSpecs [Spec]
  deriving (Eq, Ord, Show)

propertyParser :: Parser (Text, RawField)
propertyParser = do
  remote <- findFieldName
  _ <- chunk ":"
  value <- textValue
  pure (remote, value)
  where
    findFieldName :: Parser Text
    findFieldName = takeWhileP (Just "field name") (/= ':')

    textValue :: Parser RawField
    textValue = do
      _ <- chunk " "
      RawText <$> restOfLine

specPropertyParser :: Parser (Text, RawField)
specPropertyParser = L.indentBlock scn $ do
  remote <- findFieldName
  _ <- chunk ":"
  pure $ L.IndentMany Nothing (\a -> pure (remote, RawSpecs a)) specParser
  where
    findFieldName :: Parser Text
    findFieldName = takeWhileP (Just "field name") (/= ':')

isEndLine :: Char -> Bool
isEndLine '\n' = True
isEndLine '\r' = True
isEndLine _ = False

specParser :: Parser Spec
specParser = L.indentBlock scn p
  where
    p = do
      name <- findDep
      version <- findVersion
      pure (L.IndentMany Nothing (pure . Spec version name) specDepParser)

specDepParser :: Parser SpecDep
specDepParser = do
  name <- findDep
  _ <- ignored
  pure $ SpecDep name

scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ some (char ' ')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

findDep :: Parser Text
findDep = lexeme (takeWhile1P (Just "dep") (not . C.isSpace))

findVersion :: Parser Text
findVersion = do
  _ <- char '('
  result <- lexeme (takeWhileP (Just "version") (/= ')'))
  _ <- char ')'
  pure result

dependenciesSectionParser :: Parser Section
dependenciesSectionParser = L.nonIndented scn $
  L.indentBlock scn $ do
    _ <- chunk "DEPENDENCIES"
    pure $ L.IndentMany Nothing (pure . DependencySection) findDependency

findDependency :: Parser DirectDep
findDependency = do
  dep <- findDep
  _ <- ignored
  pure $ DirectDep dep
