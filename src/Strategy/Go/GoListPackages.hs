-- |
-- Module : Strategy.Gomodules.GoListPackages
--
-- Description : Analyze a Go project using go list -json -deps all
module Strategy.Go.GoListPackages (analyze) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, fatal)
import Data.String.Conversion (toText, ToText)
import DepTypes (Dependency)
import Effect.Exec (AllowErr (Never), Command (Command, cmdAllowErr, cmdArgs, cmdName), Exec, execThrow, renderCommand, ExecErr (CommandParseError))
import Graphing qualified
import Path (Abs, Dir, Path)
import Types (GraphBreadth)
import Strategy.Go.Transitive (decodeMany)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON (parseJSON), (.:?), (.:), (.!=), withObject)
import Data.Aeson.Internal (formatError)
import Control.Applicative ((<|>))


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


newtype ModulePath = ModulePath Text
  deriving (Eq, Ord, Show, ToText, Generic, Hashable)

instance FromJSON ModulePath
  
newtype ModuleVersion = ModuleVersion Text
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
  deriving (Eq, Show, Generic)

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

getDeps :: [GoPackage] -> m (Graphing.Graphing Dependency, GraphBreadth)
getDeps = undefined
