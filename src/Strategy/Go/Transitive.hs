module Strategy.Go.Transitive (
  fillInTransitive,
  graphTransitive,
  normalizeImportsToModules,
  Module (..),
  Package (..),
) where

import Control.Algebra
import Control.Applicative (many)
import Control.Monad (unless)
import Control.Effect.Diagnostics
import Data.Aeson
import Data.Aeson.Internal (formatError, iparse)
import Data.Aeson.Parser
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Effect.Exec
import Effect.Grapher
import Path
import Strategy.Go.Types
import qualified Data.Map.Strict as M

goListCmd :: Command
goListCmd = Command
  { cmdName = "go"
  , cmdArgs = ["list", "-json", "all"]
  , cmdAllowErr = NonEmptyStdout
  }

data Package = Package
  { packageImportPath :: Text
  , packageModule     :: Maybe Module
  , packageImports    :: Maybe [Text]
  , packageSystem     :: Maybe Bool
  } deriving (Eq, Ord, Show)

data Module = Module
  { modPath    :: Text
  , modVersion :: Maybe Text
  } deriving (Eq, Ord, Show)

instance FromJSON Package where
  parseJSON = withObject "Package" $ \obj ->
    Package <$> obj .:  "ImportPath"
            <*> obj .:? "Module"
            <*> obj .:? "Imports"
            <*> obj .:? "Standard"

instance FromJSON Module where
  parseJSON = withObject "Module" $ \obj ->
    Module <$> obj .:  "Path"
           <*> obj .:? "Version"

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

graphTransitive :: Has GolangGrapher sig m => [Package] -> m ()
graphTransitive = void . traverse_ go
  where
  go :: Has GolangGrapher sig m => Package -> m ()
  go package = unless (packageSystem package == Just True) $ do
    let -- when a gomod field is present, use that for the package import path
        -- otherwise use the top-level package import path
        path :: Text
        path = maybe (packageImportPath package) modPath (packageModule package)

        pkg :: GolangPackage
        pkg = mkGolangPackage path

    traverse_ (traverse_ (edge pkg . mkGolangPackage)) (packageImports package)

    -- when we have a gomod, and that gomod has a version, add label for version
    case modVersion =<< packageModule package of
      Nothing -> pure ()
      Just ver -> label pkg (mkGolangVersion ver)

fillInTransitive ::
  ( Has GolangGrapher sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path x Dir ->
  m ()
fillInTransitive dir = do
  goListOutput <- execThrow dir goListCmd
  case decodeMany goListOutput of
    Left (path, err) -> fatal (CommandParseError goListCmd (T.pack (formatError path err)))
    Right (packages :: [Package]) -> graphTransitive (normalizeImportsToModules packages)

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
  normalizeSingle :: Package -> Package
  normalizeSingle package = package{packageImports = map replaceImport <$> packageImports package}

  -- If a package doesn't have an associated module, use the package name instead
  replaceImport :: Text -> Text
  replaceImport package = Maybe.fromMaybe package (M.lookup package packageNameToModule)

  packageNameToModule :: M.Map Text Text
  packageNameToModule =
    M.fromList
      [ (packageImportPath package, modPath gomod)
      | package <- packages
      , Just gomod <- [packageModule package]
      ]
