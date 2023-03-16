-- |
-- Module : Strategy.Gomodules.GoListPackages
--
-- Description : Analyze a Go project using go list -json -deps all
module Strategy.Go.GoListPackages (analyze) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, fatal)
import Data.String.Conversion (toText, ToText)
import Effect.Exec (AllowErr (Never), Command (Command, cmdAllowErr, cmdArgs, cmdName), Exec, execThrow, renderCommand, ExecErr (CommandParseError))
import Graphing qualified
import Path (Abs, Dir, Path)
import Types (GraphBreadth (Complete))
import Strategy.Go.Transitive (decodeMany)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON (parseJSON), (.:?), (.:), (.!=), withObject)
import Data.Aeson.Internal (formatError)
import Control.Applicative ((<|>))
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (foldl')
import Effect.Grapher (Grapher, edge, evalGrapher, deep, direct)
import Data.Foldable (traverse_)
import qualified Control.Effect.Diagnostics as Diagnostics
import Control.Monad ((>=>), unless)
import DepTypes (Dependency (..), DepType (GoType), VerConstraint (CEq), DepEnvironment (EnvProduction))


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
  deriving (Eq, Ord, Show, ToText, Generic, Hashable)

instance FromJSON ModulePath

newtype ModuleVersion = ModuleVersion {unModuleVersion :: Text}
  deriving (Eq, Ord, Show, ToText, Generic, Hashable)

instance FromJSON ModuleVersion

data GoModule = GoModule {
  modulePath :: ModulePath
  -- The main go module will be unversioned
  , version :: Maybe ModuleVersion
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
  { modName :: ModulePath,
    modVersion :: Maybe ModuleVersion
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable ModuleSummary
data DepSummary =
  Indirect ModuleSummary
  | Direct ModuleSummary
  | Root ModuleSummary
  | NonMod ImportPath
  deriving (Eq, Ord, Show)

-- * Analysis

goListCmd :: Command
goListCmd =
  Command
    { cmdName = "go",
      cmdArgs = ["list", "-json", "-deps", "all"],
      cmdAllowErr = Never
    }

analyze ::
  ( Has Exec sig m,
    Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m (Graphing.Graphing Dependency, GraphBreadth)
analyze goModDir = do
  stdout <- context ("Getting dependencies using '" <> renderCommand goListCmd <> "'") $ execThrow goModDir goListCmd
  case decodeMany stdout of
    Left (path, err) -> fatal $ CommandParseError goListCmd (toText (formatError path err))
    Right pkgs -> do
      context "Analyzing dependencies" $ getDeps pkgs

getDeps :: (Has Diagnostics sig m) => [GoPackage] -> m (Graphing.Graphing Dependency, GraphBreadth)
getDeps = fmap (, Complete) . getPackageData

-- this is a bad name and wrong type, but as I work incrementally I will change it.
getPackageData :: (Has Diagnostics sig m) => [GoPackage] -> m (Graphing.Graphing Dependency)
getPackageData rawPackages = evalGrapher $ do
  traverse_ graphEdges pkgsNoStdLibImports
  where
    (stdLibImportPaths, pkgsNoStdLibImports) = foldl' go (HashSet.empty, HashMap.empty) rawPackages

    go :: (HashSet.HashSet ImportPath , HashMap.HashMap ImportPath GoPackage) -> GoPackage -> (HashSet.HashSet ImportPath, HashMap.HashMap ImportPath GoPackage)
    go (stdLibPaths, otherPackages) g@GoPackage{standard, importPath}
      | standard  = (HashSet.insert importPath stdLibPaths, otherPackages)
      | otherwise = (stdLibPaths, HashMap.insert importPath (removeStdLibDeps g) otherPackages)

    removeStdLibDeps :: GoPackage -> GoPackage
    removeStdLibDeps g@GoPackage {packageDeps = pDeps} =
      g {packageDeps = filter (\i -> not $ i `HashSet.member` stdLibImportPaths) pDeps}

    graphEdges :: (Has Diagnostics sig m, Has (Grapher Dependency) sig m) => GoPackage -> m ()
    graphEdges GoPackage {importPath, packageDeps} = do
      modInfo@GoModule{isMainModule, indirect} <- importToModule importPath
      unless isMainModule $ do
        let modDep = modToDep modInfo
        traverse_ (importToModule >=> edge modDep . modToDep) packageDeps
        if indirect then
          deep modDep
          else
          direct modDep

    modToDep :: GoModule -> Dependency
    modToDep (GoModule{modulePath
                      , version})
      = Dependency
          {dependencyType = GoType
          , dependencyName = unModulePath modulePath
          , dependencyVersion = CEq . unModuleVersion <$> version
          , dependencyLocations = []
          , dependencyEnvironments = Set.singleton EnvProduction -- Fix this for test deps in the futures
          , dependencyTags = Map.empty}

    -- Look up module info for an import path, performing replacements if necessary.
    importToModule :: Has Diagnostics sig m => ImportPath -> m GoModule
    importToModule impPath = do
          -- Return a stand-in value for missing import paths so it's visible to users? even if unscannable.
          Diagnostics.fromMaybe ("No module for " <> toText impPath) $ do
            GoPackage {moduleInfo = modInfo} <- HashMap.lookup impPath pkgsNoStdLibImports
            (modInfo >>= replacement) <|> modInfo
