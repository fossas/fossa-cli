{-# LANGUAGE DerivingVia #-}

-- |
-- Module : Strategy.Gomodules.GoListPackages
--
-- Description : Analyze a Go project using go list -json -deps all
module Strategy.Go.GoListPackages (analyze
                                  -- Exported for testing only
                                  , buildGraph) where

import Control.Algebra (Has)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, context, fatal)
import Control.Effect.Diagnostics qualified as Diagnostics
import Control.Monad (unless, when)
import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:), (.:?))
import Data.Aeson.Internal (formatError)
import Data.Foldable (traverse_)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion (ToText, toText)
import Data.Text (Text, isPrefixOf)
import DepTypes (DepEnvironment (EnvProduction), DepType (GoType), Dependency (..), VerConstraint (CEq))
import Effect.Exec (AllowErr (Never), Command (Command, cmdAllowErr, cmdArgs, cmdName), Exec, ExecErr (CommandParseError), execThrow, renderCommand)
import Effect.Grapher (Grapher, deep, direct, edge, evalGrapher)
import GHC.Generics (Generic)
import Graphing qualified
import Path (Abs, Dir, Path)
import Strategy.Go.Transitive (decodeMany)
import Types (GraphBreadth (Complete))
import Graphing (pruneUnreachable)

-- * Types

-- |Path used in a Go project to import a package.
newtype ImportPath = ImportPath Text
  deriving (Eq, Ord, Show, ToText, Generic, Hashable)

instance FromJSON ImportPath

data GoPackage = GoPackage
  { importPath :: ImportPath
  , standard :: Bool
  , moduleInfo :: Maybe GoModule
  , packageDeps :: [ImportPath]
  }
  deriving (Show)

instance FromJSON GoPackage where
  parseJSON = withObject "GoPackage" $
    \obj ->
      GoPackage
        <$> obj .: "ImportPath"
        <*> obj .:? "Standard" .!= False
        -- Once stdlib packages are eliminated we should generally have a module
        <*> obj .:? "Module"
        -- I think that imports is the correct key here as opposed to "Deps"
        -- "Deps" includes recursively imported packages as well.
        -- Those should have their own entries in the output though.
        <*> (obj .: "Imports" <|> pure [])

newtype ModulePath = ModulePath {unModulePath :: Text}
  deriving (Eq, Ord, Show, ToText, FromJSON, Hashable)

newtype ModuleVersion = ModuleVersion {unModuleVersion :: Text}
  deriving (Eq, Ord, Show, ToText, FromJSON, Hashable)

data GoModule = GoModule
  { modulePath :: ModulePath
  , -- The main go module will be unversioned
    version :: Maybe ModuleVersion
  , indirect :: Bool
  , isMainModule :: Bool
  , replacement :: Maybe GoModule
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable GoModule

instance FromJSON GoModule where
  parseJSON = withObject "Go Module" $
    \obj ->
      GoModule
        <$> obj .: "Path"
        <*> obj .:? "Version"
        <*> obj .:? "Indirect" .!= False
        <*> obj .:? "Main" .!= False
        <*> obj .:? "Replace"

-- remove the following two types later.
-- This data should be in Dependency and graph labels.
data ModuleSummary = ModuleSummary
  { modName :: ModulePath
  , modVersion :: Maybe ModuleVersion
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable ModuleSummary
data DepSummary
  = Indirect ModuleSummary
  | Direct ModuleSummary
  | Root ModuleSummary
  | NonMod ImportPath
  deriving (Eq, Ord, Show)

-- * Analysis

goListCmd :: Command
goListCmd =
  Command
    { cmdName = "go"
    , cmdArgs = ["list", "-json", "-deps", "all"]
    , cmdAllowErr = Never
    }

analyze ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m (Graphing.Graphing Dependency, GraphBreadth)
analyze goModDir = do
  stdout <- context ("Getting dependencies using '" <> renderCommand goListCmd <> "'") $ execThrow goModDir goListCmd
  case decodeMany stdout of
    Left (path, err) -> fatal $ CommandParseError goListCmd (toText (formatError path err))
    Right pkgs -> do
      context "Analyzing dependencies" $ buildGraph pkgs

buildGraph :: (Has Diagnostics sig m) => [GoPackage] -> m (Graphing.Graphing Dependency, GraphBreadth)
buildGraph = fmap (,Complete) . getPackageData

-- | Given a list of GoPackages, generate a graph of *module* dependencies based on their package dependencies.
-- This works by first making a mapping of package import paths to modules using the provided packages' 'moduleInfo' field.
-- Next each package's deps have edges made between a it and its dependencies, except that the function uses a package's parent module to generate a the vertex for the package in the graph.
-- The end result is a graph of module dependencies for only modules that have at least one of its packages used by the built project.
--
-- Other features of this function:
-- * Removes standard lib deps where they are called out by a flag.
--   Stdlib packages with pseudo versions are not filtered yet. This is the same behavior as the current dynamic tactic in Strategy.Go.GoList.
-- * Removes path dependencies and child deps that aren't used elsewhere in the graph.
--   The go tools give us this data but we haven't decided yet how to present it.
-- * Replaces modules according to the module's 'replacement' field.
getPackageData :: (Has Diagnostics sig m) => [GoPackage] -> m (Graphing.Graphing Dependency)
getPackageData rawPackages= fmap pruneUnreachable -- Remove deps that are only children of removed path deps.
                             . evalGrapher $ do
  traverse_ graphEdges pkgsNoStdLibImports
  where
    (stdLibImportPaths, pkgsNoStdLibImports) = foldl' go (HashSet.empty, HashMap.empty) rawPackages

    go :: (HashSet.HashSet ImportPath, HashMap.HashMap ImportPath GoPackage) -> GoPackage -> (HashSet.HashSet ImportPath, HashMap.HashMap ImportPath GoPackage)
    go (stdLibPaths, otherPackages) g@GoPackage{standard, importPath}
      | standard = (HashSet.insert importPath stdLibPaths, otherPackages)
      | otherwise = (stdLibPaths, HashMap.insert importPath (removeStdLibDeps g) otherPackages)

    removeStdLibDeps :: GoPackage -> GoPackage
    removeStdLibDeps g@GoPackage{packageDeps = pDeps} =
      g{packageDeps = filter (\i -> not $ i `HashSet.member` stdLibImportPaths) pDeps}

    graphEdges :: (Has Diagnostics sig m, Has (Grapher Dependency) sig m) => GoPackage -> m ()
    graphEdges GoPackage{importPath, packageDeps} = do
      currModule@GoModule{isMainModule, indirect} <- importToModule importPath
      unless (isMainModule || isPathDep currModule) $ do
        let modDep = modToDep currModule
        let addChildEdge :: (Has Diagnostics sig m, Has (Grapher Dependency) sig m) => ImportPath -> m ()
            addChildEdge p = do pMod <- importToModule p
                                when (pMod /= currModule) $
                                  edge modDep (modToDep pMod)
        traverse_ addChildEdge packageDeps
        if indirect
          then deep modDep
          else direct modDep

    -- Look up module info for an import path, performing replacements if necessary.
    importToModule :: Has Diagnostics sig m => ImportPath -> m GoModule
    importToModule impPath = do
      -- Return a stand-in value for missing import paths so it's visible to users? even if unscannable.
      Diagnostics.fromMaybe ("No module for " <> toText impPath) $ do
        GoPackage{moduleInfo = modInfo} <- HashMap.lookup impPath pkgsNoStdLibImports
        (modInfo >>= replacement) <|> modInfo

-- |A module is a path dep if its import path starts with './' or '../'.
isPathDep :: GoModule -> Bool
-- I think this method of detection is crude, but I'm not sure how else to distinguish path deps from regular ones.
-- If I don't assume that a path dep always starts with one of these prefixes, it's possible that what looks like an import path is actually a relative path to a local directory.
-- One possible improvement is to only perform this check when a module is a replacement for another.
-- Is this too unix specific?
isPathDep GoModule{modulePath=ModulePath mP} = any (`isPrefixOf` mP) ["./", "../"]

modToDep :: GoModule -> Dependency
modToDep
  ( GoModule
      { modulePath = ModulePath t
      , version
      }
    ) =
    Dependency
      { dependencyType = GoType
      , dependencyName = t
      , dependencyVersion = CEq . unModuleVersion <$> version
      , dependencyLocations = []
      , dependencyEnvironments = Set.singleton EnvProduction
      , dependencyTags = Map.empty
      }
