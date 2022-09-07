module Strategy.NuGet.Paket (
  discover,
  findProjects,
  getDeps,
  mkProject,
  findSections,
  buildGraph,
  PaketDep (..),
  Section (..),
  Remote (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  run,
 )
import Control.Effect.Reader (Reader)
import Control.Monad (guard)
import Data.Aeson (ToJSON)
import Data.Char qualified as C
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import DepTypes (
  DepType (NuGetType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Grapher (
  LabeledGrapher,
  direct,
  edge,
  label,
  withLabeling,
 )
import Effect.ReadFS (ReadFS, readContentsParser)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path, parent)
import Text.Megaparsec (
  MonadParsec (eof, takeWhile1P, takeWhileP, try),
  Parsec,
  between,
  chunk,
  empty,
  many,
  some,
  (<|>),
 )
import Text.Megaparsec.Char (char, eol, space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (PaketProjectType),
  GraphBreadth (Complete),
 )

type Parser = Parsec Void Text

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject PaketProject]
discover = simpleDiscover findProjects mkProject PaketProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [PaketProject]
findProjects = walkWithFilters' $ \_ _ files -> do
  case findFileNamed "paket.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([PaketProject file], WalkContinue)

newtype PaketProject = PaketProject
  { paketLock :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PaketProject

instance AnalyzeProject PaketProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = getDeps

mkProject :: PaketProject -> DiscoveredProject PaketProject
mkProject project =
  DiscoveredProject
    { projectType = PaketProjectType
    , projectBuildTargets = mempty
    , projectPath = parent $ paketLock project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => PaketProject -> m DependencyResults
getDeps = context "Paket" . context "Static analysis" . analyze' . paketLock

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m DependencyResults
analyze' file = do
  sections <- readContentsParser findSections file
  graph <- context "Building dependency graph" $ pure (buildGraph sections)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [file]
      }

newtype PaketPkg = PaketPkg {pkgName :: Text}
  deriving (Eq, Ord, Show)

type PaketGrapher = LabeledGrapher PaketPkg PaketLabel

data PaketLabel
  = PaketVersion Text
  | PaketRemote Text
  | GroupName Text
  | DepLocation Text
  deriving (Eq, Ord, Show)

toDependency :: PaketPkg -> Set PaketLabel -> Dependency
toDependency pkg = foldr applyLabel start
  where
    start :: Dependency
    start =
      Dependency
        { dependencyType = NuGetType
        , dependencyName = pkgName pkg
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

    applyLabel :: PaketLabel -> Dependency -> Dependency
    applyLabel (PaketVersion ver) dep = dep{dependencyVersion = Just (CEq ver)}
    applyLabel (GroupName name) dep = dep{dependencyTags = Map.insertWith (++) "group" [name] (dependencyTags dep)}
    applyLabel (DepLocation loc) dep = dep{dependencyTags = Map.insertWith (++) "location" [loc] (dependencyTags dep)}
    applyLabel (PaketRemote repo) dep = dep{dependencyLocations = repo : dependencyLocations dep}

buildGraph :: [Section] -> Graphing Dependency
buildGraph sections =
  run . withLabeling toDependency $
    traverse_ (addSection "MAIN") sections
  where
    addSection :: Has PaketGrapher sig m => Text -> Section -> m ()
    addSection group (StandardSection location remotes) = traverse_ (addRemote group location) remotes
    addSection _ (GroupSection group gSections) = traverse_ (addSection group) gSections
    addSection _ (UnknownSection _) = pure ()

    addRemote :: Has PaketGrapher sig m => Text -> Text -> Remote -> m ()
    addRemote group loc remote = traverse_ (addSpec (DepLocation loc) (PaketRemote $ remoteLocation remote) (GroupName group)) (remoteDependencies remote)

    addSpec :: Has PaketGrapher sig m => PaketLabel -> PaketLabel -> PaketLabel -> PaketDep -> m ()
    addSpec loc remote group dep = do
      -- add edges, labels, and direct deps
      let pkg = PaketPkg (depName dep)
      traverse_ (edge pkg . PaketPkg) (transitive dep)
      label pkg (PaketVersion (depVersion dep))
      label pkg remote
      label pkg group
      label pkg loc
      direct pkg

type Name = Text
type Location = Text

data Section
  = StandardSection Location [Remote]
  | UnknownSection Text
  | GroupSection Name [Section]
  deriving (Eq, Ord, Show)

data Remote = Remote
  { remoteLocation :: Text
  , remoteDependencies :: [PaketDep]
  }
  deriving (Eq, Ord, Show)

data PaketDep = PaketDep
  { depName :: Text
  , depVersion :: Text
  , transitive :: [Text]
  }
  deriving (Eq, Ord, Show)

data Group = Group
  { groupName :: Text
  , groupSections :: [Section]
  }
  deriving (Eq, Ord, Show)

findSections :: Parser [Section]
findSections = many (try standardSectionParser <|> try groupSection <|> try unknownSection) <* eof

groupSection :: Parser Section
groupSection = do
  _ <- chunk "GROUP"
  name <- textValue
  sections <- many (try standardSectionParser <|> try unknownSection)
  pure $ GroupSection name sections

unknownSection :: Parser Section
unknownSection = do
  emptyLine <- restOfLine
  guard $ not $ "GROUP" `Text.isPrefixOf` emptyLine
  _ <- eol
  pure $ UnknownSection emptyLine

standardSectionParser :: Parser Section
standardSectionParser = L.nonIndented scn $
  L.indentBlock scn $ do
    location <- chunk "HTTP" <|> "GITHUB" <|> "NUGET"
    pure $ L.IndentMany Nothing (pure . StandardSection location) remoteParser

remoteParser :: Parser Remote
remoteParser = L.indentBlock scn $ do
  _ <- chunk "remote:"
  value <- textValue
  pure $ L.IndentMany Nothing (pure . Remote value) specParser

specParser :: Parser PaketDep
specParser = L.indentBlock scn $ do
  name <- findDep
  version <- findVersion
  _ <- restOfLine
  pure $ L.IndentMany Nothing (pure . PaketDep name version) specsParser

specsParser :: Parser Text
specsParser = findDep <* restOfLine

findDep :: Parser Text
findDep = lexeme (takeWhile1P (Just "dep") (not . C.isSpace))

findVersion :: Parser Text
findVersion = between (char '(') (char ')') $ takeWhile1P (Just "version") (/= ')')

textValue :: Parser Text
textValue = chunk " " *> restOfLine

restOfLine :: Parser Text
restOfLine = takeWhileP (Just "ignored") (not . isEndLine)

isEndLine :: Char -> Bool
isEndLine '\n' = True
isEndLine '\r' = True
isEndLine _ = False

scn :: Parser ()
scn = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space (void $ some (char ' ')) empty empty
