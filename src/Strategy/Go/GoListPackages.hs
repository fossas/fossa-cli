-- |
-- Module : Strategy.Gomodules.GoListPackages
--
-- Description : Analyze a Go project using go list -json -deps all
module Strategy.Go.GoListPackages (
  analyze,

  -- * Exported for testing only
  buildGraph,
  GoPackage (..),
  ImportPath (..),
  GoModule (..),
  ModulePath (..),
  ModuleVersion (..),
) where

import Control.Algebra (Has)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, context, fatal)
import Control.Effect.Diagnostics qualified as Diagnostics
import Control.Effect.State (gets)
import Control.Monad (unless, (<=<), (>=>))
import Data.Aeson (FromJSON (parseJSON), Value, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Internal (formatError)
import Data.Foldable (traverse_)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion (ToText, decodeUtf8, toText)
import Data.Text (Text, isPrefixOf)
import DepTypes (
  DepEnvironment (..),
  DepType (GoType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Exec (AllowErr (Never), Command (Command, cmdAllowErr, cmdArgs, cmdName), Exec, ExecErr (CommandParseError), execThrow, renderCommand)
import Effect.Grapher (Grapher, LabeledGrapherC, deep, direct, edge, label, withLabeling)
import GHC.Generics (Generic)
import Graphing qualified
import Path (Abs, Dir, Path)
import Prettyprinter (pretty)
import Strategy.Go.Transitive (decodeMany)
import Types (GraphBreadth (Complete))

-- * Types

-- | Path used in a Go project to import a package.
newtype ImportPath = ImportPath Text
  deriving (Eq, Ord, Show, ToText, Generic, Hashable)

instance FromJSON ImportPath

newtype GoListPackageError = GoListPackageError Value
  deriving (Eq, Show, FromJSON)

instance ToDiagnostic GoListPackageError where
  renderDiagnostic (GoListPackageError v) =
    pretty @Text $
      "'go list -json -deps all' reported a package error: \n"
        <> (decodeUtf8 . encodePretty $ v)
        <> "\nThis may affect analysis results for this package, but often FOSSA can still analyze it."
        <> "\nVerify the analysis results for the affected package on fossa.com."

data GoPackage = GoPackage
  { importPath :: ImportPath
  , standard :: Bool
  , moduleInfo :: Maybe GoModule
  , packageDeps :: [ImportPath]
  , listError :: Maybe GoListPackageError
  , testDeps :: [ImportPath]
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
        <*> obj .:? "Error"
        <*> (obj .:? "TestImports" .!= [])

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

-- * Analysis

goListCmd :: Command
goListCmd =
  Command
    { cmdName = "go"
    , cmdArgs =
        [ "list"
        , "-e" -- Note malformed or not found packages in the GoPackage "Error" field and return a 0 exit-status.
        -- During testing there were some packages reported as erroneous due to how they were used in the project, but it is still possible to analyze them.
        -- Rather than fail and fall back to static analysis, this tactic will still try to upload those packages and report the error as a warning.
        -- Not doing this would make us do static analysis on projects that previously worked with the `go mod graph` based tactic.
        -- See https://github.com/kubernetes/kubernetes/tree/master/hack/tools for a project needing this behavior.
        -- See https://pkg.go.dev/cmd/go/internal/list for documentation of this flag
        , "-json" -- Output in json format
        , "-deps" -- Recursively print deps
        , "all" -- all packages
        ]
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

type GoLabeledGrapher m a = LabeledGrapherC Dependency DepEnvironment m a

-- | Given a list of GoPackages, generate a graph of *module* dependencies based on their package dependencies.
-- This works by first making a mapping of package import paths to modules using the provided packages' 'moduleInfo' field.
-- Next each package's deps have edges made between a it and its dependencies, except that the function uses a package's parent module to generate a the vertex for the package in the graph.
-- The end result is a graph of module dependencies for only modules that have at least one of its packages used by the built project.
--
-- Other features of this function:
-- * Removes standard lib deps where they are called out by a flag.
--   Stdlib packages with pseudo versions are not filtered yet. This is the same behavior as the current dynamic tactic in Strategy.Go.GoList.
-- * Removes path dependencies and their transitive deps that aren't used elsewhere in the graph.
--   The go tools give us this data but we haven't decided yet how to present it.
-- * Replaces modules according to the module's 'replacement' field.
buildGraph :: (Has Diagnostics sig m) => [GoPackage] -> m (Graphing.Graphing Dependency, GraphBreadth)
buildGraph rawPackages =
  fmap (,Complete)
   . withLabeling labeler
   . traverse_ (makeGraph EnvProduction <=< lookupPackage)
   . packageDeps
    =<< Diagnostics.fromMaybe MissingMainModuleErr maybeMainPackage
  where
    (maybeMainPackage, stdLibImportPaths, pkgsNoStdLibImports) = foldl' go (Nothing, HashSet.empty, HashMap.empty) rawPackages

    go :: (Maybe GoPackage, HashSet.HashSet ImportPath, HashMap.HashMap ImportPath GoPackage) -> GoPackage -> (Maybe GoPackage, HashSet.HashSet ImportPath, HashMap.HashMap ImportPath GoPackage)
    go (maybeMain, stdLibPaths, otherPackages) g@GoPackage{standard, importPath, moduleInfo}
      | standard = (maybeMain, HashSet.insert importPath stdLibPaths, otherPackages)
      | Just GoModule{isMainModule = True} <- moduleInfo =
          let g' = removeIgnoredPackages g in
            (Just g', stdLibPaths, HashMap.insert importPath g' otherPackages)
      | otherwise = (maybeMain, stdLibPaths, HashMap.insert importPath (removeIgnoredPackages g) otherPackages)

    -- "C" is a special package for using Go's FFI.
    -- Ignore it because trying to look it up as a package import path later will fail.
    ignoredPackages :: HashSet.HashSet ImportPath
    ignoredPackages = HashSet.insert (ImportPath "C") stdLibImportPaths

    removeIgnoredPackages :: GoPackage -> GoPackage
    removeIgnoredPackages
      g@GoPackage
        { packageDeps = pDeps
        , testDeps = tDeps
        } =
        g
          { packageDeps = filter (\i -> not $ i `HashSet.member` ignoredPackages) pDeps
          , testDeps = filter (\i -> not $ i `HashSet.member` ignoredPackages) tDeps
          }

    -- Graph childPkg and its children with parentPkg as its parent, returning the Dependency for child if it was graphed.
    makeGraph :: (Has Diagnostics sig m) => DepEnvironment -> GoPackage -> GoLabeledGrapher m (Maybe Dependency)
    makeGraph currentEnv GoPackage{importPath, packageDeps, moduleInfo} = do
      currentMod@GoModule{indirect} <- case (moduleInfo >>= replacement) <|> moduleInfo of
        Just m -> pure m
        Nothing -> fatal $ MissingModuleErr importPath

      if (isPathDep currentMod)
        then pure Nothing
        else do
          let pkgDep = modToDep currentMod
          -- This is a depth-first post-order traversal of an acyclic graph.
          -- So if a vertex exists, its children have also exist so skip graphing them a second time.
          existsAlready <- gets (Graphing.hasVertex pkgDep)
          unless existsAlready $
            traverse_ (lookupPackage >=> makeGraph currentEnv >=> maybeEdge pkgDep) packageDeps

          if indirect then deep pkgDep else direct pkgDep
          label pkgDep currentEnv
          pure . Just $ pkgDep

    maybeEdge :: (Has Diagnostics sig m, Has (Grapher Dependency) sig m) => Dependency -> Maybe Dependency -> m ()
    maybeEdge d = maybe (pure ()) (edge d)

    lookupPackage :: Has Diagnostics sig m => ImportPath -> m GoPackage
    lookupPackage impPath = Diagnostics.fromMaybe (MissingModuleErr impPath) $ HashMap.lookup impPath pkgsNoStdLibImports

labeler :: Dependency -> Set.Set DepEnvironment -> Dependency
labeler dep envs = dep{dependencyEnvironments = envs}

newtype MissingModuleErr = MissingModuleErr ImportPath
  deriving (Eq, Show)

instance ToDiagnostic MissingModuleErr where
  renderDiagnostic (MissingModuleErr (ImportPath i)) = pretty $ "Could not find module for " <> i

data MissingMainModuleErr = MissingMainModuleErr
  deriving (Eq, Show)

-- TODO: better error message
instance ToDiagnostic MissingMainModuleErr where
  renderDiagnostic _ = pretty @Text $ "No main module for project"

-- | A module is a path dep if its import path starts with './' or '../'.
isPathDep :: GoModule -> Bool
-- Checking for ./ or ../ is the documented way of detecting path deps.
-- https://go.dev/ref/mod#go-mod-file-replace
isPathDep GoModule{modulePath = ModulePath mP} = any (`isPrefixOf` mP) ["./", "../"]

modToDep :: GoModule -> Dependency
modToDep
  GoModule
    { modulePath = ModulePath modPath
    , version
    } =
    Dependency
      { dependencyType = GoType
      , dependencyName = modPath
      , dependencyVersion = CEq . unModuleVersion <$> version
      , dependencyLocations = []
      , dependencyEnvironments = Set.empty -- These will be set using labels
      , dependencyTags = Map.empty
      }
