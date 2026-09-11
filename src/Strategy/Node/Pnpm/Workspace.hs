module Strategy.Node.Pnpm.Workspace (PnpmWorkspace (..), resolveCatalogReferences) where

import Control.Effect.Diagnostics (Diagnostics, Has, recover)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:?))
import Data.Glob (Glob)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Walk (findFileInAncestor)
import Effect.ReadFS (ReadFS, readContentsYaml)
import Path (Abs, File, Path, Rel, parent)
import Strategy.Node.PackageJson (PackageJson (..))

newtype PnpmWorkspace = PnpmWorkspace {workspaceSpecs :: [Glob Rel]}
  deriving (Eq, Ord, Show)

instance FromJSON PnpmWorkspace where
  parseJSON = withObject "Pnpm Workspace" $
    \o -> PnpmWorkspace <$> o .:? "packages" .!= []

-- Parse catalogs separately so malformed catalog data cannot affect workspace
-- discovery or lockfile analysis, neither of which needs these declarations.
newtype WorkspaceCatalogs = WorkspaceCatalogs (Map Text (Map Text Text))

instance FromJSON WorkspaceCatalogs where
  parseJSON = withObject "Pnpm workspace catalogs" $ \o -> do
    defaultCatalog <- o .:? "catalog"
    namedCatalogs <- o .:? "catalogs" .!= mempty
    pure $ WorkspaceCatalogs $ maybe namedCatalogs (\catalog -> Map.insert "default" catalog namedCatalogs) defaultCatalog

-- | Expand catalog references before package.json-only graph construction.
-- The nearest workspace file also covers scans started inside a member. Never
-- search beyond that file for a missing catalog: a nested workspace owns its
-- own catalog namespace. Values are declared constraints, not locked versions.
resolveCatalogReferences :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> PackageJson -> m (PackageJson, Maybe (Path Abs File))
resolveCatalogReferences manifest pkg
  | not (any (Text.isPrefixOf "catalog:") (Map.elems (packageDeps pkg) <> Map.elems (packageDevDeps pkg))) = pure (pkg, Nothing)
  | otherwise = do
      result <- recover $ do
        file <- findFileInAncestor (parent manifest) "pnpm-workspace.yaml"
        catalogs <- readContentsYaml file
        pure (resolve catalogs pkg, Just file)
      pure $ fromMaybe (pkg, Nothing) result
  where
    resolve (WorkspaceCatalogs catalogs) package =
      package
        { packageDeps = Map.mapWithKey (resolveVersion catalogs) (packageDeps package)
        , packageDevDeps = Map.mapWithKey (resolveVersion catalogs) (packageDevDeps package)
        }

    resolveVersion catalogs name version = fromMaybe version $ do
      catalog <- Text.stripPrefix "catalog:" version
      entries <- Map.lookup (if Text.null catalog then "default" else catalog) catalogs
      Map.lookup name entries
