{-# LANGUAGE OverloadedRecordDot #-}

module Strategy.Node.Bun.BunLock (
  analyze,
  parseBunLock,
  buildGraph,
  BunLockFile (..),
  BunWorkspace (..),
  BunPackage (..),
)
where

import Control.Algebra (run)
import Control.Effect.Diagnostics (Diagnostics, Has, context, fatal)
import Data.Aeson (
  FromJSON (parseJSON),
  Result (..),
  Value (..),
  eitherDecodeStrict,
  fromJSON,
  withArray,
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion (encodeUtf8, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import DepTypes (DepEnvironment (..), DepType (NodeJSType), Dependency (..), VerConstraint (CEq))
import Effect.Grapher (deep, direct, edge, evalGrapher)
import Effect.ReadFS (ReadFS, ReadFSErr (FileParseError), readContentsText)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, File, Path)

-- | Bun Lockfile structure
-- Bun lockfile (bun.lock) is a JSONC format with the following shape:
--
-- @
-- > {
-- >   "lockfileVersion": 1,
-- >   "workspaces": {
-- >     "": {
-- >       "name": "my-project",
-- >       "dependencies": {
-- >         "lodash": "^4.17.21"
-- >       },
-- >       "devDependencies": {
-- >         "typescript": "^5.0.0"
-- >       }
-- >     }
-- >   },
-- >   "packages": {
-- >     "lodash": ["lodash@4.17.21", "", {}, "sha512-xxx"],
-- >     "typescript": ["typescript@5.3.3", "", {"bin": {"tsc": "bin/tsc"}}, "sha512-yyy"]
-- >   }
-- > }
-- @
--
-- In this file:
--   * `lockfileVersion`: Version of the lockfile format
--   * `workspaces`: Map of workspace configurations
--     * Key (e.g. "") refers to workspace path
--     * `name`: Workspace name
--     * `dependencies`: Direct production dependencies
--     * `devDependencies`: Direct development dependencies
--   * `packages`: Map of all resolved packages
--     * Key: Package name (e.g. "lodash")
--     * Value: Array [resolution, registry, info, integrity]
--       - resolution: Package@version string
--       - registry: Registry URL (empty string for npm)
--       - info: Object with optional bin, scripts, etc.
--       - integrity: SHA512 hash
data BunLockFile = BunLockFile
  { lockfileVersion :: Int
  , workspaces :: Map Text BunWorkspace
  , packages :: Map Text BunPackage
  }
  deriving (Show, Eq)

data BunWorkspace = BunWorkspace
  { name :: Text
  , dependencies :: Map Text Text
  , devDependencies :: Map Text Text
  }
  deriving (Show, Eq, Ord)

data BunPackage = BunPackage
  { resolution :: Text
  , registry :: Text
  , info :: Value
  , integrity :: Text
  }
  deriving (Show, Eq)

-- | FromJSON instance for BunLockFile
-- Parses the top-level bun.lock structure
instance FromJSON BunLockFile where
  parseJSON = withObject "BunLockFile" $ \obj ->
    BunLockFile
      <$> obj .: "lockfileVersion"
      <*> obj .:? "workspaces" .!= mempty
      <*> obj .:? "packages" .!= mempty

-- | FromJSON instance for BunWorkspace
-- Parses workspace configuration
instance FromJSON BunWorkspace where
  parseJSON = withObject "BunWorkspace" $ \obj ->
    BunWorkspace
      <$> obj .:? "name" .!= ""
      <*> obj .:? "dependencies" .!= mempty
      <*> obj .:? "devDependencies" .!= mempty

-- | FromJSON instance for BunPackage
-- Parses the array format: [resolution, registry, info, integrity]
instance FromJSON BunPackage where
  parseJSON = withArray "BunPackage" $ \arr -> do
    let vec = V.toList arr
    case vec of
      [resVal, regVal, infoVal, integrityVal] -> do
        res <- parseJSON resVal
        reg <- parseJSON regVal
        info' <- parseJSON infoVal
        integrity' <- parseJSON integrityVal
        pure $ BunPackage res reg info' integrity'
      _ -> fail $ "Expected array with 4 elements, got " ++ show (length vec)

-- | Parse a bun.lock file
-- Bun lockfiles use JSONC format (JSON with comments)
-- This function strips comments before parsing
parseBunLock ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  Path Abs File ->
  m BunLockFile
parseBunLock file = context ("Parsing bun.lock file '" <> toText (show file) <> "'") $ do
  contents <- readContentsText file
  let stripped = stripJsoncComments contents
      bs = encodeUtf8 stripped
  case eitherDecodeStrict bs of
    Left err -> fatal $ FileParseError (show file) (toText err)
    Right lockFile -> pure lockFile

-- | Convert JSONC to valid JSON
-- JSONC (JSON with Comments) allows:
-- 1. Single-line comments starting with //
-- 2. Trailing commas before } or ]
--
-- This function strips both to produce valid JSON
stripJsoncComments :: Text -> Text
stripJsoncComments input = removeTrailingCommas $ Text.unlines $ map processLine $ Text.lines input
  where
    -- Process a single line: strip comments
    processLine :: Text -> Text
    processLine line =
      let stripped = Text.stripStart line
       in if "//" `Text.isPrefixOf` stripped
            then ""
            else stripInlineComment line

    -- Strip inline comments (// outside of strings)
    stripInlineComment :: Text -> Text
    stripInlineComment = go False
      where
        go :: Bool -> Text -> Text
        go _ t | Text.null t = t
        go inString t =
          case Text.uncons t of
            Nothing -> t
            Just ('"', rest)
              | not inString -> "\"" <> go True rest
              | otherwise -> "\"" <> go False rest
            Just ('\\', rest)
              | inString ->
                  -- Escaped char in string, take next char too
                  case Text.uncons rest of
                    Just (c, rest') -> "\\" <> Text.singleton c <> go True rest'
                    Nothing -> "\\"
              | otherwise -> "\\" <> go inString rest
            Just ('/', rest)
              | not inString ->
                  case Text.uncons rest of
                    Just ('/', _) -> "" -- Comment found, strip rest of line
                    _ -> "/" <> go inString rest
              | otherwise -> "/" <> go inString rest
            Just (c, rest) -> Text.singleton c <> go inString rest

    -- Remove trailing commas before } or ]
    -- Pattern: comma followed by optional whitespace then } or ]
    removeTrailingCommas :: Text -> Text
    removeTrailingCommas = go False
      where
        go :: Bool -> Text -> Text
        go _ t | Text.null t = t
        go inString t =
          case Text.uncons t of
            Nothing -> t
            Just ('"', rest)
              | not inString -> "\"" <> go True rest
              | otherwise -> "\"" <> go False rest
            Just ('\\', rest)
              | inString ->
                  case Text.uncons rest of
                    Just (c, rest') -> "\\" <> Text.singleton c <> go True rest'
                    Nothing -> "\\"
              | otherwise -> "\\" <> go inString rest
            Just (',', rest)
              | not inString ->
                  -- Check if this comma is followed by whitespace then } or ]
                  let afterWs = Text.dropWhile (`elem` [' ', '\t', '\n', '\r']) rest
                   in case Text.uncons afterWs of
                        Just ('}', _) -> go False rest -- Skip the comma
                        Just (']', _) -> go False rest -- Skip the comma
                        _ -> "," <> go False rest -- Keep the comma
              | otherwise -> "," <> go inString rest
            Just (c, rest) -> Text.singleton c <> go inString rest

-- | Build a dependency graph from a parsed bun lockfile
--
-- The graph building process:
-- 1. Iterate over all workspaces to mark direct dependencies
-- 2. For each workspace dependency, look up the resolved package and mark as direct
-- 3. Dev dependencies are marked with EnvDevelopment, production with EnvProduction
-- 4. Iterate over all packages to add deep dependencies and edges
-- 5. Extract transitive dependencies from package info and create edges
buildGraph :: BunLockFile -> Graphing Dependency
buildGraph lockFile = run . evalGrapher $ do
  -- Collect all dev dependency names from all workspaces
  let devDepNames = Set.fromList $ concatMap (Map.keys . devDependencies) (Map.elems lockFile.workspaces)

  -- Process all workspaces for direct dependencies
  for_ (Map.elems lockFile.workspaces) $ \workspace -> do
    -- Production dependencies
    for_ (Map.keys workspace.dependencies) $ \depName -> do
      case Map.lookup depName lockFile.packages of
        Nothing -> pure ()
        Just pkg -> direct $ packageToDep pkg False

    -- Dev dependencies
    for_ (Map.keys workspace.devDependencies) $ \depName -> do
      case Map.lookup depName lockFile.packages of
        Nothing -> pure ()
        Just pkg -> direct $ packageToDep pkg True

  -- Process all packages for deep dependencies and edges
  for_ (Map.toList lockFile.packages) $ \(_, pkg) -> do
    let isDev = isDevDep devDepNames pkg
        parentDep = packageToDep pkg isDev

    -- Add as deep dependency
    deep parentDep

    -- Extract dependencies from info and create edges
    let pkgDeps = extractDependencies pkg.info
    for_ (Map.keys pkgDeps) $ \childName -> do
      case Map.lookup childName lockFile.packages of
        Nothing -> pure ()
        Just childPkg -> do
          let childDep = packageToDep childPkg (isDevDep devDepNames childPkg)
          edge parentDep childDep
  where
    -- Check if a package is a dev dependency based on the collected dev dep names
    isDevDep :: Set.Set Text -> BunPackage -> Bool
    isDevDep devNames pkg =
      let (name, _) = parseResolution pkg.resolution
       in Set.member name devNames

-- | Convert a BunPackage to a Dependency
packageToDep :: BunPackage -> Bool -> Dependency
packageToDep pkg isDev =
  let (name, version) = parseResolution pkg.resolution
      env = if isDev then EnvDevelopment else EnvProduction
   in Dependency
        { dependencyType = NodeJSType
        , dependencyName = name
        , dependencyVersion = Just (CEq version)
        , dependencyLocations = mempty
        , dependencyEnvironments = Set.singleton env
        , dependencyTags = mempty
        }

-- | Parse resolution string "name@version" or "@scope/name@version"
--
-- >>> parseResolution "lodash@4.17.21"
-- ("lodash", "4.17.21")
--
-- >>> parseResolution "@angular/core@16.0.0"
-- ("@angular/core", "16.0.0")
parseResolution :: Text -> (Text, Text)
parseResolution res
  | "@" `Text.isPrefixOf` res =
      -- Scoped package: @scope/name@version
      let withoutAt = Text.drop 1 res
          (scopeAndName, rest) = Text.breakOn "@" withoutAt
       in ("@" <> scopeAndName, Text.drop 1 rest)
  | otherwise =
      -- Regular package: name@version
      let (name, rest) = Text.breakOn "@" res
       in (name, Text.drop 1 rest)

-- | Extract dependencies map from package info Value
-- The info object may contain a "dependencies" key with a map of dep name to version spec
extractDependencies :: Value -> Map Text Text
extractDependencies (Object obj) =
  case KM.lookup "dependencies" obj of
    Just depsVal ->
      case fromJSON depsVal of
        Success deps -> deps
        Error _ -> mempty
    Nothing -> mempty
extractDependencies _ = mempty

-- | Filter out workspace packages from the dependency graph
-- Workspace packages are internal packages in a monorepo and should not be included
-- in the final dependency graph
filterWorkspaces :: BunLockFile -> Graphing Dependency -> Graphing Dependency
filterWorkspaces lockFile =
  let workspaceNames = Set.fromList $ map name (Map.elems lockFile.workspaces)
   in Graphing.shrink (\dep -> not (Set.member (dependencyName dep) workspaceNames))

-- | Analyze a bun.lock file and produce a dependency graph
analyze ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  Path Abs File ->
  m (Graphing Dependency)
analyze file = do
  lockfile <- parseBunLock file
  pure $ filterWorkspaces lockfile $ buildGraph lockfile
