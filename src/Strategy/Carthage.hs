{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.Carthage
  ( discover
  , findProjects
  , getDeps
  , mkProject
  , analyze
  , ResolvedEntry(..)
  , EntryType(..)
  ) where

import Control.Effect.Diagnostics
import Data.Char (isSpace)
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import DepTypes
import Discovery.Walk
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing as G
import Path
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = map mkProject <$> findProjects dir

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [CarthageProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "Cartfile.resolved" files of
    Nothing -> pure ([], WalkContinue)
    Just cartfile -> do

      let project =
            CarthageProject
              { carthageLock = cartfile,
                carthageDir = dir
              }

      pure ([project], WalkSkipAll)

data CarthageProject = CarthageProject
  { carthageDir :: Path Abs Dir
  , carthageLock :: Path Abs File
  } deriving (Eq, Ord, Show)

mkProject :: (Has ReadFS sig n, Has Diagnostics sig n) => CarthageProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "carthage",
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ getDeps project,
      projectPath = carthageDir project,
      projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => CarthageProject -> m (Graphing Dependency)
getDeps = fmap (G.gmap toDependency) . analyze . carthageLock

relCheckoutsDir :: Path Abs File -> Path Abs Dir
relCheckoutsDir file = parent file </> $(mkRelDir "Carthage/Checkouts")

analyze ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  )
  => Path Abs File -> m (G.Graphing ResolvedEntry)
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
    )
    => Path Abs File -> m [ResolvedEntry]
  analyzeSingle path = do
    entries <- readContentsParser @[ResolvedEntry] resolvedCartfileParser path
    traverse_ (descend (relCheckoutsDir path)) entries
    pure entries

  descend ::
    ( Has ReadFS sig m
    , Has Diagnostics sig m
    , Has (Grapher ResolvedEntry) sig m
    )
    => Path Abs Dir {- checkouts directory -} -> ResolvedEntry -> m ()
  descend checkoutsDir entry = do
    let checkoutName = T.unpack $ entryToCheckoutName entry

    case parseRelDir checkoutName of
      Nothing -> pure ()
      Just path -> do
        let checkoutPath :: Path Abs Dir
            checkoutPath = checkoutsDir </> path

        deeper <- recover $ analyzeSingle (checkoutPath </> $(mkRelFile "Cartfile.resolved"))
        traverse_ (traverse_ (edge entry)) deeper

entryToCheckoutName :: ResolvedEntry -> Text
entryToCheckoutName entry =
  case resolvedType entry of
    GitEntry -> resolvedName entry
    -- this is safe because T.splitOn always returns a non-empty list
    GithubType -> last . T.splitOn "/" $ resolvedName entry
    BinaryType -> resolvedName entry

entryToDepName :: ResolvedEntry -> Text
entryToDepName entry =
  case resolvedType entry of
    GitEntry -> resolvedName entry
    GithubType -> "https://github.com/" <> resolvedName entry
    BinaryType -> resolvedName entry

toDependency :: ResolvedEntry -> Dependency
toDependency entry = Dependency
  { dependencyType = CarthageType
  , dependencyName = entryToDepName entry
  , dependencyVersion = Just (CEq (resolvedVersion entry))
  , dependencyTags = M.empty
  , dependencyEnvironments = []
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
  entryType <- lexeme $ choice
    [ GithubType <$ chunk "github"
    , GitEntry    <$ chunk "git"
    , BinaryType <$ chunk "binary"
    ]

  entryName    <- word
  entryVersion <- word
  _ <- scn

  pure (ResolvedEntry entryType entryName entryVersion)

word :: Parser Text
word = T.pack <$> choice
  [ lexeme (char '\"' *> someTill (satisfy (not . isSpace)) (char '\"'))
  , lexeme (some (satisfy (not . isSpace)))
  ]

scn :: Parser ()
scn =  L.space space1 empty empty

sc :: Parser ()
sc =  L.space (void $ some (char ' ')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

type Parser = Parsec Void Text

data ResolvedEntry = ResolvedEntry
  { resolvedType    :: EntryType
  , resolvedName    :: Text
  , resolvedVersion :: Text
  } deriving (Eq, Ord, Show)

data EntryType = GithubType | GitEntry | BinaryType
  deriving (Eq, Ord, Show)
