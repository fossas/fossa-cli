module Strategy.Node.Pnpm.Workspace (PnpmWorkspace (..), resolveCatalogReferences) where

import Control.Effect.Diagnostics (Diagnostics, Has, recover)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:?))
import Data.Glob (Glob)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Discovery.Walk (findFileInAncestor)
import Effect.ReadFS (ReadFS, readContentsYaml)
import Path (Abs, File, Path, Rel, parent)
import Strategy.Node.PackageJson (PackageJson (..))
import Strategy.Node.Pnpm.Types (WorkspaceCatalogs (..), resolveCatalogVersion)

newtype PnpmWorkspace = PnpmWorkspace {workspaceSpecs :: [Glob Rel]}
  deriving (Eq, Ord, Show)

instance FromJSON PnpmWorkspace where
  parseJSON = withObject "Pnpm Workspace" $
    \o -> PnpmWorkspace <$> o .:? "packages" .!= []

-- | Expand catalog references before package.json-only graph construction.
-- The nearest workspace file also covers scans started inside a member. Never
-- search beyond that file for a missing catalog: a nested workspace owns its
-- own catalog namespace. Values are declared constraints, not locked versions.
--
-- Catalogs are read separately from 'PnpmWorkspace' so malformed catalog
-- data cannot affect workspace discovery, which does not need them.
resolveCatalogReferences :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> PackageJson -> m (PackageJson, Maybe (Path Abs File))
resolveCatalogReferences manifest pkg
  | not (any (Text.isPrefixOf "catalog:") (Map.elems (packageDeps pkg) <> Map.elems (packageDevDeps pkg))) = pure (pkg, Nothing)
  | otherwise = do
      result <- recover $ do
        file <- findFileInAncestor (parent manifest) "pnpm-workspace.yaml"
        WorkspaceCatalogs catalogs <- readContentsYaml file
        pure (resolve catalogs pkg, Just file)
      pure $ fromMaybe (pkg, Nothing) result
  where
    resolve catalogs package =
      package
        { packageDeps = Map.mapWithKey (resolveCatalogVersion catalogs) (packageDeps package)
        , packageDevDeps = Map.mapWithKey (resolveCatalogVersion catalogs) (packageDevDeps package)
        }
