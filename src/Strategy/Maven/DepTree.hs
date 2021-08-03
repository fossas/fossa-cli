module Strategy.Maven.DepTree (
  analyze,
  parseDotGraph,

  -- * Exported for testing
  DotGraph (..),
  PackageId (..),
) where

import Control.Algebra (Has, run)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.Lift (Lift, sendIO)
import Data.Char (isSpace)
import Data.Foldable (for_, traverse_)
import Data.String (IsString)
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import DepTypes (
  DepEnvironment (..),
  DepType (MavenType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walk',
 )
import Effect.Exec (AllowErr (Never), Command (..), Exec, execThrow)
import Effect.Grapher (direct, edge, evalGrapher)
import Effect.ReadFS (ReadFS, readContentsParser)
import Graphing (Graphing, gmap)
import Path (Abs, Dir, File, Path)
import Path.IO (removeFile)
import System.Random (randomIO)
import Text.Megaparsec (
  Parsec,
  between,
  many,
  takeWhile1P,
  try,
 )
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Types (GraphBreadth (Complete))

outputFileName :: IsString a => a
outputFileName = "fossa-deptree.dot"

analyze :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => Path Abs Dir -> m (Graphing Dependency, GraphBreadth)
analyze dir = do
  -- We generate files with a random prefix so we don't pick up files from previous runs.
  -- If we did pick them up, they're likely not enough of a conflict to worry about, but
  -- it's better than not doing it.
  randIdent <- abs <$> sendIO randomIO
  _ <- context "Running maven 'dependency:tree' plugin" $ execThrow dir $ deptreeCmd randIdent
  graphFiles <- context "Locating maven output files" $ findDepTreeOutputs dir randIdent
  graphs <- context "Parsing output files" $ traverse (readContentsParser parseDotGraph) graphFiles
  -- TODO: Will this work correctly in the face of exceptions?
  sendIO $ traverse_ removeFile graphFiles
  pure (buildGraph graphs, Complete)

findDepTreeOutputs :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Int -> m [Path Abs File]
findDepTreeOutputs dir ident = flip walk' dir $ \_ _ files -> do
  case findFileNamed (show ident <> outputFileName) files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([file], WalkContinue)

buildGraph :: [DotGraph] -> Graphing Dependency
buildGraph = gmap toDependency . foldMap toGraph

toDependency :: PackageId -> Dependency
toDependency PackageId{artifactName, artifactVersion, buildTag} =
  Dependency
    { dependencyType = MavenType
    , dependencyName = artifactName
    , dependencyVersion = Just $ CEq artifactVersion
    , dependencyLocations = []
    , dependencyEnvironments = maybe [EnvProduction] ((: []) . toBuildTag) buildTag
    , dependencyTags = mempty
    }

toGraph :: DotGraph -> Graphing PackageId
toGraph DotGraph{rootNode, edgeList} = run . evalGrapher $ do
  direct rootNode
  for_ edgeList $ uncurry edge

toBuildTag :: Text -> DepEnvironment
toBuildTag = \case
  "compile" -> EnvProduction
  "test" -> EnvTesting
  other -> EnvOther other

deptreeCmd :: Int -> Command
deptreeCmd ident =
  Command
    { cmdName = "mvn"
    , cmdArgs =
        [ "dependency:tree" -- run the command
        , "-DoutputType=dot" -- export DOT format
        , "-DoutputFile=" <> toText (show ident) <> outputFileName -- dump files to these names in the relevant directories.
        ]
    , cmdAllowErr = Never
    }

data PackageId = PackageId
  { groupName :: Text
  , artifactName :: Text
  , artifactType :: Text
  , artifactVersion :: Text
  , buildTag :: Maybe Text
  }
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

data DotGraph = DotGraph
  { rootNode :: PackageId
  , edgeList :: [(PackageId, PackageId)]
  }
  deriving (Eq, Ord, Show)

parseDotGraph :: Parser DotGraph
parseDotGraph = do
  root <- symbol "digraph" *> parseNode
  edgeLists <- enclosed "{" "}" (many $ try parseGraphEntry)
  pure $ DotGraph root edgeLists

parseGraphEntry :: Parser (PackageId, PackageId)
parseGraphEntry = do
  from <- parseNode
  to <- (symbol "->") *> parseNode
  _ <- symbol ";"
  pure (from, to)

parseNode :: Parser PackageId
parseNode = lexeme ((quotedName <|> unquotedName) >>= parseName)
  where
    quotedName = enclosed "\"" "\"" $ takeWhile1P (Just "node name") (/= '\"')
    unquotedName = takeWhile1P (Just "node name") (not . isSpace)

parseName :: forall m. MonadFail m => Text -> m PackageId
parseName input = combine parts
  where
    parts :: [Text]
    parts = Text.splitOn ":" input

    combine :: [Text] -> m PackageId
    combine [a, b, c, d, e] = pure . PackageId a b c d $ Just e
    combine [a, b, c, d] = pure $ PackageId a b c d Nothing
    combine items = fail $ toString $ "invalid identifier: " <> Text.intercalate ":" items

enclosed :: Text -> Text -> Parser a -> Parser a
enclosed open close = between (symbol open) (symbol close)

symbol :: Text -> Parser Text
symbol = Lexer.symbol scn

scn :: Parser ()
scn = Lexer.space space1 (Lexer.skipLineComment "//" <|> Lexer.skipLineComment "#") (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme scn
