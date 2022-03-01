{-# LANGUAGE TemplateHaskell #-}

module Strategy.Carthage (
  discover,
  findProjects,
  getDeps,
  mkProject,
  analyze,
  ResolvedEntry (..),
  EntryType (..),
  CarthageProject (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Effect.Diagnostics
import Data.Aeson (ToJSON)
import Data.Char (isSpace)
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.Map.Strict qualified as Map
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Text.Extra qualified as Text
import Data.Void (Void)
import DepTypes
import Discovery.Walk
import Effect.Grapher
import Effect.ReadFS
import GHC.Generics (Generic)
import Graphing qualified as G
import Path
import Prettyprinter (pretty, viaShow, vsep)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject CarthageProject]
discover dir = context "Carthage" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [CarthageProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "Cartfile.resolved" files of
    Nothing -> pure ([], WalkContinue)
    Just cartfile -> do
      let project =
            CarthageProject
              { carthageLock = cartfile
              , carthageDir = dir
              }

      pure ([project], WalkSkipAll)

data CarthageProject = CarthageProject
  { carthageDir :: Path Abs Dir
  , carthageLock :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CarthageProject

instance AnalyzeProject CarthageProject where
  analyzeProject _ = getDeps

mkProject :: CarthageProject -> DiscoveredProject CarthageProject
mkProject project =
  DiscoveredProject
    { projectType = CarthageProjectType
    , projectBuildTargets = mempty
    , projectPath = carthageDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => CarthageProject -> m DependencyResults
getDeps project = do
  graph <- context "Carthage" . context "Static analysis" . fmap (G.gmap toDependency) . analyze . carthageLock $ project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [carthageLock project]
      }

relCheckoutsDir :: Path Abs File -> Path Abs Dir
relCheckoutsDir file = parent file </> $(mkRelDir "Carthage/Checkouts")

analyze ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs File ->
  m (G.Graphing ResolvedEntry)
analyze topPath = evalGrapher $ do
  -- We only care about top-level resolved cartfile errors, so we don't
  -- 'recover' here, but we do below in 'descend'
  topEntries <- analyzeSingle topPath

  for_ topEntries $ \entry -> do
    direct entry
    descend (relCheckoutsDir topPath) entry
  where
    analyzeSingle ::
      ( Has ReadFS sig m
      , Has Diagnostics sig m
      , Has (Grapher ResolvedEntry) sig m
      ) =>
      Path Abs File ->
      m [ResolvedEntry]
    analyzeSingle path = do
      entries <- readContentsParser @[ResolvedEntry] resolvedCartfileParser path
      traverse_ (descend (relCheckoutsDir path)) entries
      pure entries

    descend ::
      ( Has ReadFS sig m
      , Has Diagnostics sig m
      , Has (Grapher ResolvedEntry) sig m
      ) =>
      Path Abs Dir {- checkouts directory -} ->
      ResolvedEntry ->
      m ()
    descend checkoutsDir entry = do
      let checkoutName = toString $ entryToCheckoutName entry

      case parseRelDir checkoutName of
        Nothing -> pure ()
        Just path -> do
          let checkoutPath :: Path Abs Dir
              checkoutPath = checkoutsDir </> path

          deeper <-
            recover
              . warnOnErr (MissingCarthageDeepDep entry)
              . errCtx (MissingResolvedFile $ checkoutPath </> $(mkRelFile "Cartfile.resolved"))
              $ analyzeSingle (checkoutPath </> $(mkRelFile "Cartfile.resolved"))
          traverse_ (traverse_ (edge entry)) deeper

newtype MissingCarthageDeepDep = MissingCarthageDeepDep ResolvedEntry
instance ToDiagnostic MissingCarthageDeepDep where
  renderDiagnostic (MissingCarthageDeepDep entry) = pretty $ "Failed to find transitive dependencies for: " <> (resolvedName entry)

newtype MissingResolvedFile = MissingResolvedFile (Path Abs File)
instance ToDiagnostic MissingResolvedFile where
  renderDiagnostic (MissingResolvedFile path) =
    vsep
      [ "We could not find or parse resolved file in: " <> (viaShow path)
      , ""
      , "Ensure your carthage project is built prior to running fossa."
      ]

entryToCheckoutName :: ResolvedEntry -> Text
entryToCheckoutName entry =
  case resolvedType entry of
    GitEntry -> resolvedName entry
    -- this is safe because Text.splitOn always returns a non-empty list
    GithubType -> snd . Text.splitOnceOnEnd "/" $ resolvedName entry
    BinaryType -> resolvedName entry

entryToDepName :: ResolvedEntry -> Text
entryToDepName entry =
  case resolvedType entry of
    GitEntry -> resolvedName entry
    GithubType -> "https://github.com/" <> resolvedName entry
    BinaryType -> resolvedName entry

toDependency :: ResolvedEntry -> Dependency
toDependency entry =
  Dependency
    { dependencyType = CarthageType
    , dependencyName = entryToDepName entry
    , dependencyVersion = Just (CEq (resolvedVersion entry))
    , dependencyTags = Map.empty
    , dependencyEnvironments = mempty
    , dependencyLocations = [] -- TODO: git location?
    }

-- Exemplary Cartfile.resolved:
-- https://github.com/Carthage/Carthage/blob/8b34a90f607c5da85c75fee1f0cf20ae742bdeb2/Tests/CarthageKitTests/Resources/TestResolvedCartfile.resolved
--
-- Additional documentation for cartfile can be found here:
-- https://github.com/Carthage/Carthage/blob/master/Documentation/Artifacts.md
resolvedCartfileParser :: Parser [ResolvedEntry]
resolvedCartfileParser = many parseSingleEntry <* eof

parseSingleEntry :: Parser ResolvedEntry
parseSingleEntry = L.nonIndented scn $ do
  entryType <-
    lexeme $
      choice
        [ GithubType <$ chunk "github"
        , GitEntry <$ chunk "git"
        , BinaryType <$ chunk "binary"
        ]

  entryName <- word
  entryVersion <- word
  _ <- scn

  pure (ResolvedEntry entryType entryName entryVersion)

word :: Parser Text
word =
  toText
    <$> choice
      [ lexeme (char '\"' *> someTill (satisfy (not . isSpace)) (char '\"'))
      , lexeme (some (satisfy (not . isSpace)))
      ]

scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ some (char ' ')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

type Parser = Parsec Void Text

data ResolvedEntry = ResolvedEntry
  { resolvedType :: EntryType
  , resolvedName :: Text
  , resolvedVersion :: Text
  }
  deriving (Eq, Ord, Show)

data EntryType = GithubType | GitEntry | BinaryType
  deriving (Eq, Ord, Show)
