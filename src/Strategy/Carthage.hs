{-# LANGUAGE TemplateHaskell #-}

module Strategy.Carthage
  ( discover
  , analyze
  , ResolvedEntry(..)
  , EntryType(..)
  ) where

import qualified Prelude as Unsafe
import Prologue

import Control.Carrier.Error.Either
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Discovery.Walk
import DepTypes
import Effect.Grapher
import Effect.ReadFS
import qualified Graphing as G
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ subdirs files ->
  case find (\f -> fileName f == "Cartfile.resolved") files of
    Nothing -> walkContinue
    Just file -> do
      runSimpleStrategy "carthage-lock" CarthageGroup $ fmap (mkProjectClosure file) (analyze file)
      walkSkipAll subdirs

mkProjectClosure :: Path Rel File -> G.Graphing ResolvedEntry -> ProjectClosureBody
mkProjectClosure file graph = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = G.gmap toDependency graph
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = Complete
    }

relCheckoutsDir :: Path Rel File -> Path Rel Dir
relCheckoutsDir file = parent file </> $(mkRelDir "Carthage/Checkouts")

analyze ::
  ( Has ReadFS sig m
  , Has (Error ReadFSErr) sig m
  , Effect sig
  )
  => Path Rel File -> m (G.Graphing ResolvedEntry)
analyze topPath = evalGrapher $ do
  -- We only care about top-level resolved cartfile errors
  topEntries <- fromEither =<< analyzeSingle topPath

  for_ topEntries $ \entry -> do
    direct entry
    descend (relCheckoutsDir topPath) entry

  where

  analyzeSingle ::
    ( Has ReadFS sig m
    , Has (Grapher ResolvedEntry) sig m
    , Effect sig
    )
    => Path Rel File -> m (Either ReadFSErr [ResolvedEntry])
  analyzeSingle path = do
    maybeEntries :: Either ReadFSErr [ResolvedEntry] <- readContentsParser' resolvedCartfileParser path

    for maybeEntries $ \entries -> do
      traverse_ (descend (relCheckoutsDir path)) entries
      pure entries

  descend ::
    ( Has ReadFS sig m
    , Has (Grapher ResolvedEntry) sig m
    , Effect sig
    )
    => Path Rel Dir {- checkouts directory -} -> ResolvedEntry -> m ()
  descend checkoutsDir entry = do
    let checkoutName = T.unpack $ entryToCheckoutName entry

    case parseRelDir checkoutName of
      Nothing -> pure ()
      Just path -> do
        let checkoutPath :: Path Rel Dir
            checkoutPath = checkoutsDir </> path

        deeper <- analyzeSingle (checkoutPath </> $(mkRelFile "Cartfile.resolved"))
        traverse_ (traverse_ (edge entry)) deeper

entryToCheckoutName :: ResolvedEntry -> Text
entryToCheckoutName entry =
  case resolvedType entry of
    GitType -> resolvedName entry
    -- this is safe because T.splitOn always returns a non-empty list
    GithubType -> Unsafe.last . T.splitOn "/" $ resolvedName entry
    BinaryType -> resolvedName entry

entryToDepName :: ResolvedEntry -> Text
entryToDepName entry =
  case resolvedType entry of
    GitType -> resolvedName entry
    GithubType -> "https://github.com/" <> resolvedName entry
    BinaryType -> resolvedName entry

toDependency :: ResolvedEntry -> Dependency
toDependency entry = Dependency
  { dependencyType = CarthageType
  , dependencyName = entryToDepName entry
  , dependencyVersion = Just (CEq (resolvedVersion entry))
  , dependencyTags = M.empty
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
    , GitType    <$ chunk "git"
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
  } deriving (Eq, Ord, Show, Generic)

data EntryType = GithubType | GitType | BinaryType
  deriving (Eq, Ord, Show, Generic)
