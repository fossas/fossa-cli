
module Strategy.Ruby.GemfileLock
  ( discover
  , strategy
  , analyze
  , configure
  , findSections

  , Spec(..)
  , SpecDep(..)
  , DirectDep(..)
  , Section(..)
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Char as C
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output

import           DepTypes
import           Discovery.Walk
import           Effect.LabeledGrapher
import           Effect.ReadFS
import           Graphing (Graphing)
import           Types
import           Text.Megaparsec hiding (label)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

discover :: Discover
discover = Discover
  { discoverName = "gemfilelock"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  for_ files $ \f ->
    when (fileName f == "Gemfile.lock") $ output (configure f)
  walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "gemfile-lock"
  , strategyAnalyze = \opts -> analyze & fileInputParser findSections (targetFile opts)
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts

type Remote = Text
type Revision = Text
type Branch = Text

data Section =
      GitSection Remote (Maybe Revision) (Maybe Branch) [Spec]
      | GemSection Remote [Spec]
      | PathSection Remote [Spec]
      | DependencySection [DirectDep]
      | UnknownSection Text
        deriving (Eq, Ord, Show, Generic)

data Spec = Spec
      { specVersion :: Text
      , specName :: Text
      , specDeps :: [SpecDep]
      } deriving (Eq, Ord, Show, Generic)

newtype SpecDep = SpecDep
      { depName :: Text
      } deriving (Eq, Ord, Show, Generic)

newtype DirectDep = DirectDep
      { directName :: Text
      } deriving (Eq, Ord, Show, Generic)

analyze :: Member (Input [Section]) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

data GemfilePkg = GemfilePkg { pkgName :: Text }
  deriving (Eq, Ord, Show, Generic)

type instance PkgLabel GemfilePkg = GemfileLabel

data GemfileLabel =
    GemfileVersion Text
  | GitRemote Text (Maybe Text) -- ^ repo url, revision
  | OtherRemote Text -- ^ url
  deriving (Eq, Ord, Show, Generic)

toDependency :: GemfilePkg -> Set GemfileLabel -> Dependency
toDependency pkg = foldr applyLabel start
  where

  start :: Dependency
  start = Dependency
    { dependencyType = GemType
    , dependencyName = pkgName pkg
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyTags = M.empty
    }

  applyLabel :: GemfileLabel -> Dependency -> Dependency
  applyLabel (GemfileVersion ver) dep = dep { dependencyVersion = Just (CEq ver) }
  applyLabel (GitRemote repo maybeRevision) dep =
    dep { dependencyLocations = maybe repo (\revision -> repo <> "@" <> revision) maybeRevision : dependencyLocations dep }
  applyLabel (OtherRemote loc) dep =
    dep { dependencyLocations = loc : dependencyLocations dep }

buildGraph :: [Section] -> Graphing Dependency
buildGraph sections = run . withLabeling toDependency $
  traverse_ addSection sections
  where
  addSection :: Member (LabeledGrapher GemfilePkg) r => Section -> Sem r ()
  addSection (DependencySection deps) = traverse_ (direct . GemfilePkg . directName) deps
  addSection (GitSection remote revision branch specs) =
    traverse_ (addSpec (GitRemote remote (revision <|> branch))) specs
  addSection (PathSection remote specs) =
    traverse_ (addSpec (OtherRemote remote)) specs
  addSection (GemSection remote specs) =
    traverse_ (addSpec (OtherRemote remote)) specs
  addSection UnknownSection{} = pure ()

  addSpec :: Member (LabeledGrapher GemfilePkg) r => GemfileLabel -> Spec -> Sem r ()
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
findSections = many (try gitSectionParser <|> try gemSectionParser <|> try pathSectionParser <|> try dependenciesSectionParser <|> emptySection) <* eof

emptySection :: Parser Section
emptySection = do 
      emptyLine <- restOfLine
      _ <- eol
      pure $ UnknownSection emptyLine
      
restOfLine :: Parser Text
restOfLine = takeWhileP (Just "ignored") (not . isEndLine)

-- ignore content until the end of the line
ignored :: Parser ()
ignored = () <$ takeWhileP (Just "ignored") (not . isEndLine)

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
mkSectionParser sectionName toSection = L.nonIndented scn (L.indentBlock scn p)
      where
         p = do
            _ <- chunk sectionName
            pure $ L.IndentMany Nothing propertiesToSection (try propertyParser <|> specPropertyParser)

         propertiesToSection :: [(Text, RawField)] -> Parser Section
         propertiesToSection properties =
            let propertyMap = M.fromList properties
                result :: Either Text Section
                result = toSection propertyMap

            in case result of
                  Right x -> pure x
                  Left y -> fail $ T.unpack $ "could not parse " <> sectionName <> " section: " <> y

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right a) = Just a
eitherToMaybe (Left _) = Nothing

lookupRawText :: Text -> Map Text RawField -> Either Text Text
lookupRawText key m = let value = M.lookup key m
                  in
                  case value of
                        Just (RawText val) -> Right val
                        _ -> Left $ "a value for " <> key <> " was unable to be found in the map"

lookupRawSpecs :: Text -> Map Text RawField -> Either Text [Spec]
lookupRawSpecs key m = let value = M.lookup key m
                  in
                  case value of
                        Just (RawSpecs val) -> Right val
                        _ -> Left $ "a value for " <> key <> " was unable to be found in the map"

data RawField = RawText Text
                | RawSpecs [Spec]
                deriving (Eq, Ord, Show, Generic)

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
specPropertyParser = L.indentBlock scn p
      where 
      p = do
            remote <- findFieldName
            _ <- chunk ":"
            pure $ L.IndentMany Nothing (\a -> pure (remote, RawSpecs a)) specParser
            
      findFieldName :: Parser Text
      findFieldName = takeWhileP (Just "field name") (/= ':')

isEndLine :: Char -> Bool
isEndLine '\n' = True
isEndLine '\r' = True
isEndLine _    = False

specParser :: Parser Spec
specParser = L.indentBlock scn p 
  where
    p = do
      name <- findDep
      version <- findVersion
      return (L.IndentMany Nothing (\specs -> pure $ Spec version name specs) specDepParser)

specDepParser :: Parser SpecDep
specDepParser = do
      name <- findDep
      _ <- ignored
      pure $ SpecDep name

scn :: Parser ()
scn =  L.space space1 empty empty

sc :: Parser ()
sc =  L.space (void $ some (char ' ')) empty empty

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
dependenciesSectionParser = L.nonIndented scn (L.indentBlock scn p)
                  where
                        p = do
                              _ <- chunk "DEPENDENCIES"
                              pure $ L.IndentMany Nothing (\deps -> pure $ DependencySection deps) findDependency

findDependency :: Parser DirectDep
findDependency = do
      dep <- findDep
      _ <- ignored
      pure $ DirectDep dep
