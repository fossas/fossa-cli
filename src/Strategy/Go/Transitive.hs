module Strategy.Go.Transitive (
  fillInTransitive,
  graphTransitive,
  normalizeImportPaths,
  Module (..),
  Package (..),
  GoPackageReplacement (..),
  NormalizedImportPath (..),
  GoListPkgImportPath (..),
  decodeMany,
) where

import Control.Algebra (Has)
import Control.Applicative (Alternative ((<|>)), many)
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic (..),
  context,
  errCtx,
  fatal,
 )
import Control.Monad (unless)
import Data.Aeson (
  FromJSON (parseJSON),
  JSONPath,
  Value (Array),
  withObject,
  (.:),
  (.:?),
 )
-- This is from the attoparsec-aeson package.
-- Aeson no longer uses this parser internally.
-- We should find some way to not use attoparsec-aeson in the future if possible. 
import Data.Aeson.Parser (eitherDecodeWith, json)
import Data.Aeson.Types (formatError, iparse)
import Data.Attoparsec.ByteString qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.String (IsString)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Vector qualified as V
import Effect.Exec (
  AllowErr (NonEmptyStdout),
  Command (..),
  Exec,
  ExecErr (CommandParseError),
  execThrow,
  renderCommand,
 )
import Effect.Grapher (edge, label)
import Errata (Errata (..))
import Path (Abs, Dir, Path)
import Strategy.Go.Types (
  GolangGrapher,
  GolangPackage,
  mkGolangPackage,
  mkGolangVersion,
 )

goListCmd :: Command
goListCmd =
  Command
    { cmdName = "go"
    , cmdArgs = ["list", "-json", "all"]
    , cmdAllowErr = NonEmptyStdout
    }

data Package a = Package
  { packageImportPath :: a
  , packageModule :: Maybe Module
  , packageImports :: Maybe [a]
  , packageSystem :: Maybe Bool
  }
  deriving (Eq, Ord, Show)

newtype GoListPkgImportPath = GoListPkgImportPath Text
  deriving (Eq, Ord, Show, FromJSON, IsString)

newtype NormalizedImportPath = NormalizedImportPath {unNormalizedImportPath :: Text}
  deriving (Eq, Ord, Show, IsString)

data GoPackageReplacement = GoPackageReplacement
  { pathReplacement :: Text
  , versionReplacement :: Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON GoPackageReplacement where
  parseJSON = withObject "GoPackageReplacement" $ \obj ->
    GoPackageReplacement
      <$> obj .: "Path"
      <*> obj .: "Version"

data Module = Module
  { modPath :: Text
  , modVersion :: Maybe Text
  , modReplacement :: Maybe GoPackageReplacement
  }
  deriving (Eq, Ord, Show)

instance FromJSON a => FromJSON (Package a) where
  parseJSON = withObject "Package" $ \obj ->
    Package
      <$> obj .: "ImportPath"
      <*> obj .:? "Module"
      <*> obj .:? "Imports"
      <*> obj .:? "Standard"

instance FromJSON Module where
  parseJSON = withObject "Module" $ \obj ->
    Module
      <$> obj .: "Path"
      <*> obj .:? "Version"
      <*> obj .:? "Replace"

-- `go list -json` is dumb: it outputs a bunch of raw json objects:
--     {
--       ...
--     }
--     {
--       ...
--     }
-- decodeMany is our workaround. it produces `[a]` by repeatedly parsing
-- json objects, wrapping them into `[Value]`, then decoding `[Value]`
-- into `[a]`
decodeMany :: FromJSON a => BL.ByteString -> Either (JSONPath, String) [a]
decodeMany = eitherDecodeWith parser (iparse parseJSON)
  where
    -- skipSpace is lifted from Data.Aeson.Parser.Internal
    skipSpace = A.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09

    parser = do
      (objects :: [Value]) <- many json <* skipSpace <* A.endOfInput
      pure (Array (V.fromList objects))

graphTransitive :: Has GolangGrapher sig m => [Package NormalizedImportPath] -> m ()
graphTransitive = void . traverse_ go
  where
    go :: Has GolangGrapher sig m => Package NormalizedImportPath -> m ()
    go package = unless (packageSystem package == Just True) $ do
      let packMod :: Maybe Module
          packMod = packageModule package

          pathToGolangPackage :: NormalizedImportPath -> GolangPackage
          pathToGolangPackage = mkGolangPackage . unNormalizedImportPath

          pkg :: GolangPackage
          pkg = pathToGolangPackage (packageImportPath package)

      traverse_ (traverse_ (edge pkg . pathToGolangPackage)) (packageImports package)

      case versionReplacement <$> (modReplacement =<< packMod)
        <|> (modVersion =<< packageModule package) of
        Nothing -> pure ()
        Just ver -> label pkg (mkGolangVersion ver)

fillInTransitive ::
  ( Has GolangGrapher sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m ()
fillInTransitive dir = context "Getting deep dependencies" $ do
  goListOutput <- errCtx GoListCmdFailed $ execThrow dir goListCmd
  case decodeMany goListOutput of
    Left (path, err) -> fatal (CommandParseError goListCmd (toText (formatError path err)))
    Right (packages :: [Package GoListPkgImportPath]) ->
      context "Adding transitive dependencies" $
        graphTransitive (normalizeImportPaths packages)

data GoListCmdFailed = GoListCmdFailed
instance ToDiagnostic GoListCmdFailed where
  renderDiagnostic _ = do
    let header = "We could not perform `" <> renderCommand goListCmd <> "` successfully to infer deep dependencies."
    Errata (Just header) [] Nothing

-- HACK(fossas/team-analysis#514) `go list -json all` emits golang dependencies
-- at the _package_ level; e.g., `github.com/example/foo/some/package`. The
-- fossa backend handles package-level imports poorly, especially when there are
-- many of them from the same go module. See the ticket for details.
--
-- As a workaround, we resolve import paths to their respective module or module
-- replacement, map package imports to their module or replacement, and use the
-- modules in the final dependency graph.
normalizeImportPaths :: [Package GoListPkgImportPath] -> [Package NormalizedImportPath]
normalizeImportPaths packages = map normalizeSingle packages
  where
    normalizeSingle :: Package GoListPkgImportPath -> Package NormalizedImportPath
    normalizeSingle package =
      package
        { packageImports = map normalizeImportPath <$> packageImports package
        , -- when a gomod field is present, use that or the module replacement
          -- for the package import path otherwise use the top-level package
          -- import path
          packageImportPath = normalizeImportPath $ packageImportPath package
        }

    -- If a package doesn't have an associated module, use the package name instead
    normalizeImportPath :: GoListPkgImportPath -> NormalizedImportPath
    normalizeImportPath pName@(GoListPkgImportPath pkgImportPath) =
      Maybe.fromMaybe (NormalizedImportPath pkgImportPath) (Map.lookup pName packageNameToModule)

    getModuleName :: Module -> NormalizedImportPath
    getModuleName m = NormalizedImportPath $ maybe (modPath m) pathReplacement (modReplacement m)

    packageNameToModule :: Map.Map GoListPkgImportPath NormalizedImportPath
    packageNameToModule =
      Map.fromList
        [ (packageImportPath package, getModuleName gomod)
        | package <- packages
        , Just gomod <- [packageModule package]
        ]
