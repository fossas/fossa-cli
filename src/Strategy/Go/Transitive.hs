module Strategy.Go.Transitive (
  fillInTransitive,
  graphTransitive,
  normalizeImportsToModules,
  Module (..),
  Package (..),
  GoPackageReplacement (..),
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
  json,
  withObject,
  (.:),
  (.:?),
 )
import Data.Aeson.Internal (formatError, iparse)
import Data.Aeson.Parser (eitherDecodeWith)
import Data.Attoparsec.ByteString qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
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
import Path (Abs, Dir, Path)
import Prettyprinter (pretty)
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

data Package = Package
  { packageImportPath :: Text
  , packageModule :: Maybe Module
  , packageImports :: Maybe [Text]
  , packageSystem :: Maybe Bool
  }
  deriving (Eq, Ord, Show)

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

instance FromJSON Package where
  parseJSON = withObject "Package" $ \obj ->
    Package <$> obj .: "ImportPath"
      <*> obj .:? "Module"
      <*> obj .:? "Imports"
      <*> obj .:? "Standard"

instance FromJSON Module where
  parseJSON = withObject "Module" $ \obj ->
    Module <$> obj .: "Path"
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

modPathOrReplacedPath :: Module -> Text
modPathOrReplacedPath m = maybe (modPath m) pathReplacement (modReplacement m)

graphTransitive :: Has GolangGrapher sig m => [Package] -> m ()
graphTransitive = void . traverse_ go
  where
    go :: Has GolangGrapher sig m => Package -> m ()
    go package = unless (packageSystem package == Just True) $ do
      let packMod :: Maybe Module
          packMod = packageModule package

          -- when a gomod field is present, use that or the module replacement
          -- for the package import path otherwise use the top-level package
          -- import path
          path :: Text
          path = maybe (packageImportPath package) modPathOrReplacedPath packMod

          pkg :: GolangPackage
          pkg = mkGolangPackage path

      traverse_ (traverse_ (edge pkg . mkGolangPackage)) (packageImports package)

      case versionReplacement <$> (packMod >>= modReplacement)
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
    Right (packages :: [Package]) -> context "Adding transitive dependencies" $ graphTransitive (normalizeImportsToModules packages)

data GoListCmdFailed = GoListCmdFailed
instance ToDiagnostic GoListCmdFailed where
  renderDiagnostic _ = pretty $ "We could not perform `" <> renderCommand goListCmd <> "` successfully to infer deep dependencies."

-- HACK(fossas/team-analysis#514) `go list -json all` emits golang dependencies
-- at the _package_ level; e.g., `github.com/example/foo/some/package`. The
-- fossa backend handles package-level imports poorly, especially when there are
-- many of them from the same go module. See the ticket for details.
--
-- As a workaround, we map package imports to their modules, and use the modules
-- in the final dependency graph.
normalizeImportsToModules :: [Package] -> [Package]
normalizeImportsToModules packages = map normalizeSingle packages
  where
    -- TODO: Normalize names to replacements here also
    normalizeSingle :: Package -> Package
    normalizeSingle package = package{packageImports = map replaceImport <$> packageImports package}

    -- If a package doesn't have an associated module, use the package name instead
    replaceImport :: Text -> Text
    replaceImport package = Maybe.fromMaybe package (Map.lookup package packageNameToModule)

    packageNameToModule :: Map.Map Text Text
    packageNameToModule =
      Map.fromList
        [ (packageImportPath package, modPathOrReplacedPath gomod)
        | package <- packages
        , Just gomod <- [packageModule package]
        ]
