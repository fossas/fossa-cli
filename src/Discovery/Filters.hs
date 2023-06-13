{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}

module Discovery.Filters (
  AllFilters (..),
  targetFilterParser,
  applyFilters,
  FilterCombination,
  FilterMatch (..),
  FilterResult (..),
  apply,
  filterIsVSIOnly,
  comboInclude,
  comboExclude,
  combinedPaths,
  combinedTargets,
  Include,
  Exclude,
  pathAllowed,
  toolAllowed,
  withMultiToolFilter,
  withToolFilter,
  ignoredPaths,
  isDefaultNonProductionPath,
) where

import Control.Effect.Reader (Has, Reader, ask)
import Control.Monad ((<=<))
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.List (isInfixOf, stripPrefix, (\\))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Semigroup (sconcat)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty (nonEmpty, toSet)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path, Rel, fromAbsDir, isProperPrefixOf, parseRelDir)
import Text.Megaparsec (
  MonadParsec (eof, takeWhile1P, try),
  Parsec,
  satisfy,
  some,
  (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char)
import Types (BuildTarget (..), DiscoveredProjectType, FoundTargets (FoundTargets, ProjectWithoutTargets), TargetFilter (..))

-- | filterIsVSIOnly is a very naive check for whether the user provided a filter for the VSI target *only*, at the root of the project with no path specified.
-- This decision was made because predicating VSI only scans is a stopgap until we have more broad support for *preemptive* analysis filtering.
-- VSI supports very large scans that the rest of our analysis may not.
filterIsVSIOnly :: AllFilters -> Bool
filterIsVSIOnly AllFilters{..} = do
  let includes = combinedTargets includeFilters
  length includes == 1 && any matches includes
  where
    matches f = case f of
      TypeTarget name -> name == "vsi"
      _ -> False

data AllFilters = AllFilters
  { includeFilters :: FilterCombination Include
  , excludeFilters :: FilterCombination Exclude
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON AllFilters where
  toEncoding = genericToEncoding defaultOptions

instance Semigroup AllFilters where
  (AllFilters a1 b1) <> (AllFilters a2 b2) = AllFilters (a1 <> a2) (b1 <> b2)

instance Monoid AllFilters where
  mempty = AllFilters mempty mempty

data FilterCombination a = FilterCombination
  { _combinedTargets :: [TargetFilter]
  , _combinedPaths :: [Path Rel Dir]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON (FilterCombination a) where
  toEncoding = genericToEncoding defaultOptions

type role FilterCombination nominal

data Include

data Exclude

instance Semigroup (FilterCombination a) where
  (FilterCombination a1 b1) <> (FilterCombination a2 b2) = FilterCombination (a1 <> a2) (b1 <> b2)

instance Monoid (FilterCombination a) where
  mempty = FilterCombination mempty mempty

withMultiToolFilter :: Has (Reader AllFilters) sig m => [DiscoveredProjectType] -> m [a] -> m [a]
withMultiToolFilter tools act = do
  filters <- ask
  let allowedSet = allowedTools filters
  if any (`Set.member` allowedSet) tools
    then act
    else pure mempty

withToolFilter :: Has (Reader AllFilters) sig m => DiscoveredProjectType -> m [a] -> m [a]
withToolFilter tool = withMultiToolFilter [tool]

toolAllowed :: AllFilters -> DiscoveredProjectType -> Bool
toolAllowed filters tool = tool `Set.member` allowedTools filters

allowedTools :: AllFilters -> Set DiscoveredProjectType
allowedTools AllFilters{..} = (fullSet `Set.intersection` includeSet) `Set.difference` excludeSet
  where
    fullSet = Set.fromList allTools
    includedTargets = combinedTargets includeFilters
    excludedTargets = combinedTargets excludeFilters
    excludeSet = Set.fromList $ mapMaybe ((`Map.lookup` toolNameMap) <=< extractPureTool) excludedTargets
    includeSet =
      Set.fromList $
        if null includedTargets
          then allTools -- No includes means "include everything"
          else mapMaybe ((`Map.lookup` toolNameMap) . extractTool) includedTargets

toolNameMap :: Map Text DiscoveredProjectType
toolNameMap = Map.fromList $ map (\x -> (toText x, x)) allTools

allTools :: [DiscoveredProjectType]
allTools = enumFromTo minBound maxBound

extractTool :: TargetFilter -> Text
extractTool = \case
  TypeTarget txt -> txt
  TypeDirTarget txt _ -> txt
  TypeDirTargetTarget txt _ _ -> txt

extractPureTool :: TargetFilter -> Maybe Text
extractPureTool = \case
  TypeTarget txt -> Just txt
  _ -> Nothing

-- | For path filtering, we follow a few key rules:
-- * If a path is excluded we reject that path and all of its children.
-- * If a path is included, we allow that path and all of its parents and children.
-- * If no paths are specifically included, we only reject explicitly excluded paths and their children.
-- * If a path is excluded and included, it is rejected.
-- TODO: Is it possible to allow conflicted items?  If so, is it possible without creating multiple versions of this function?
pathAllowed :: AllFilters -> Path Rel Dir -> Bool
pathAllowed AllFilters{..} path = isIncluded && not isExcluded
  where
    includeIsEmpty = null includedPaths
    isExcluded = isExcludedMember || isChildOfExcludeMember
    -- We include parents because our directory scanner will never make it to the included path without the parents
    -- We include children because our analysis filtering allows children of included members
    isIncluded = includeIsEmpty || isParentOfIncludeMember || isIncludeMember || isChildOfIncludeMember
    isIncludeMember = path `elem` includedPaths
    isExcludedMember = path `elem` excludedPaths
    isChildOfIncludeMember = any (`isProperPrefixOf` path) includedPaths
    isChildOfExcludeMember = any (`isProperPrefixOf` path) excludedPaths
    isParentOfIncludeMember = any (path `isProperPrefixOf`) includedPaths
    includedPaths = combinedPaths includeFilters
    excludedPaths = combinedPaths excludeFilters

isDefaultNonProductionPath :: Path Abs Dir -> Path Abs Dir -> Bool
isDefaultNonProductionPath baseDir projPath =
  isNonProductionPath $
    dropPrefix (fromAbsDir baseDir) (fromAbsDir projPath)
  where
    isNonProductionPath :: FilePath -> Bool
    isNonProductionPath path = any (`isInfixOf` path) ignoredPaths

    dropPrefix :: String -> String -> String
    dropPrefix prefix str = fromMaybe str (stripPrefix prefix str)

ignoredPaths :: [[Char]]
ignoredPaths =
  [ "dist-newstyle"
  , "doc/"
  , "docs/"
  , "test/"
  , "example/"
  , "examples/"
  , "vendor/"
  , "node_modules/"
  , ".venv/" -- python pdm package manager's pkgyard (similar to node_modules/)
  , ".srclib-cache/"
  , "spec/"
  , "Godeps/"
  , ".git/"
  , "bower_components/"
  , "third_party/"
  , "third-party/"
  , "Carthage/"
  , "Checkouts/"
  ]

comboInclude :: [TargetFilter] -> [Path Rel Dir] -> FilterCombination Include
comboInclude = FilterCombination

comboExclude :: [TargetFilter] -> [Path Rel Dir] -> FilterCombination Exclude
comboExclude = FilterCombination

combinedTargets :: FilterCombination a -> [TargetFilter]
combinedTargets = _combinedTargets

combinedPaths :: FilterCombination a -> [Path Rel Dir]
combinedPaths = _combinedPaths

-- applyFilters determines if legacy filters are present and if they need to converted to `TargetFilters` for filtering.
applyFilters :: AllFilters -> Text -> Path Rel Dir -> FoundTargets -> Maybe FoundTargets
applyFilters (AllFilters onlyFilters excludeFilters) tool dir = filterFoundTargets (apply onlyFilters excludeFilters tool dir)

-- finalizeResult combines a FilterResult with the targets that exist in a project and returns the final filtering Result
-- If the return value is Nothing, that means that no targets remain for scanning.
-- The second to last cases ensure that the final included targets are intersectioned with the targets that actually exist for a project.
-- The final case ensures that all excluded targets are removed from a projects final list of targets.
filterFoundTargets :: FilterResult -> FoundTargets -> Maybe FoundTargets
filterFoundTargets ResultNone _ = Nothing
filterFoundTargets _ ProjectWithoutTargets = Just ProjectWithoutTargets
filterFoundTargets ResultAll targets = Just targets
filterFoundTargets (ResultInclude found) (FoundTargets targets) = FoundTargets <$> nonEmpty (Set.intersection (toSet targets) $ Set.fromList $ NE.toList found)
filterFoundTargets (ResultExclude found) (FoundTargets targets) = FoundTargets <$> nonEmpty (Set.difference (toSet targets) $ Set.fromList $ NE.toList found)

-- (buildTargetFilters-only `union` pathFilters-only)
--   `subtract` (buildTargetFilters-exclude `union` pathFilters-exclude)
apply :: FilterCombination Include -> FilterCombination Exclude -> Text -> Path Rel Dir -> FilterResult
apply include exclude buildtool dir =
  dSubtract
    (fromMaybe MatchAll (applyComb include buildtool dir))
    (fromMaybe MatchNone (applyComb exclude buildtool dir))

-- Nothing = "Unknown" -- i.e., there were no filters that matched the buildtool + directories.
applyComb :: FilterCombination a -> Text -> Path Rel Dir -> Maybe FilterMatch
applyComb comb buildtool dir =
  buildTargetFiltersResult <> pathFiltersResult
  where
    buildTargetFiltersResult :: Maybe FilterMatch
    buildTargetFiltersResult = foldMap' (\t -> applyTarget t buildtool dir) (combinedTargets comb)

    pathFiltersResult :: Maybe FilterMatch
    pathFiltersResult = foldMap' (`applyPath` dir) (combinedPaths comb)

applyTarget :: TargetFilter -> Text -> Path Rel Dir -> FilterMatch
applyTarget (TypeTarget t) u _ = if t == u then MatchAll else MatchNone
applyTarget (TypeDirTarget t p) u q = if t == u && p == q then MatchAll else MatchNone
applyTarget (TypeDirTargetTarget t p target) u q = if t == u && p == q then MatchSome (target NE.:| []) else MatchNone

-- (parent path) (child path)
applyPath :: Path Rel Dir -> Path Rel Dir -> FilterMatch
applyPath t u = if isProperPrefixOf t u || t == u then MatchAll else MatchNone

-- MatchNone <> MatchAll = MatchAll is the reason for this order
-- (MatchSome <> MatchAll) and (MatchAll <> MatchSome) outputs the results in MatchSome.
-- The implications of this are that if a TypeTargetFilter matches all targets and
-- a TypeDirTargetTargetFilter matches a specific target we will prefer the specific target.
instance Semigroup FilterMatch where
  MatchNone <> t = t
  t <> MatchNone = t
  t <> MatchAll = t
  MatchAll <> t = t
  MatchSome ts <> MatchSome us = MatchSome (ts <> us)

-- | 'foldMap', but only requires a 'Semigroup' instance.
--
-- When the provided list is empty, this returns 'Nothing'
foldMap' :: Semigroup s => (a -> s) -> [a] -> Maybe s
foldMap' f xs = sconcat <$> NE.nonEmpty (map f xs)

data FilterMatch = MatchNone | MatchAll | MatchSome (NE.NonEmpty BuildTarget)
  deriving (Eq, Ord, Show)

data FilterResult = ResultNone | ResultAll | ResultInclude (NE.NonEmpty BuildTarget) | ResultExclude (NE.NonEmpty BuildTarget)
  deriving (Eq, Ord, Show)

-- dSubtract -> IncludeMatches -> ExcludeMatches -> FilterResult
-- dSubtract defines how different types of include and exclude matches are merged to create a FilterResult.
dSubtract :: FilterMatch -> FilterMatch -> FilterResult
dSubtract _ MatchAll = ResultNone
dSubtract MatchNone _ = ResultNone
dSubtract MatchAll MatchNone = ResultAll
dSubtract MatchAll (MatchSome xs) = ResultExclude xs
dSubtract (MatchSome xs) MatchNone = ResultInclude xs
dSubtract (MatchSome xs) (MatchSome ys) = maybe ResultNone ResultInclude (neDifference xs ys)

-- | Compute the difference of two non-empty lists
neDifference :: Eq a => NE.NonEmpty a -> NE.NonEmpty a -> Maybe (NE.NonEmpty a)
neDifference xs ys = NE.nonEmpty (NE.toList xs \\ NE.toList ys)

type Parser = Parsec Void Text

targetFilterParser :: Parser TargetFilter
targetFilterParser = (try targetFilter <|> try projectFilter <|> typeFilter) <* eof
  where
    targetFilter = TypeDirTargetTarget <$> buildtool <* char '@' <*> path <* char ':' <*> target
    projectFilter = TypeDirTarget <$> buildtool <* char '@' <*> path
    typeFilter = TypeTarget <$> buildtool

    buildtool :: Parser Text
    buildtool = toText <$> some alphaNumChar

    path :: Parser (Path Rel Dir)
    path = do
      filepath <- some (satisfy (/= ':'))
      case parseRelDir filepath of
        Left err -> fail (show err)
        Right a -> pure a

    target :: Parser BuildTarget
    target = BuildTarget <$> takeWhile1P Nothing (const True)
