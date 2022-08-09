module Strategy.AlpineLinux.Apk (analyze) where

import Container.OsRelease (OsInfo (OsInfo))
import Control.Effect.Diagnostics (
  Diagnostics,
  context,
  warn,
 )
import Data.Either (lefts, rights)
import Data.Foldable (traverse_)
import Data.String.Conversion (toText)
import DepTypes (
  DepType (LinuxAPK),
  Dependency (Dependency),
  VerConstraint (CEq),
 )
import Effect.ReadFS (
  Has,
  ReadFS,
  readContentsParser,
 )
import Graphing (
  Graphing,
  directs,
 )
import Path (Abs, Dir, File, Path)
import Strategy.AlpineLinux.Parser (installedPackagesDatabaseParser)
import Strategy.AlpineLinux.Types (
  AlpinePackage (
    alpinePackageArchitecture,
    alpinePackageName,
    alpinePackageVersion
  ),
 )
import Types (GraphBreadth (..))

buildGraph :: OsInfo -> [AlpinePackage] -> Graphing Dependency
buildGraph (OsInfo os osVersion) pkgs = directs (map toDependency pkgs)
  where
    toDependency :: AlpinePackage -> Dependency
    toDependency pkg =
      Dependency
        LinuxAPK
        (alpinePackageName pkg <> "#" <> os <> "#" <> osVersion)
        (Just $ version pkg)
        mempty
        mempty
        mempty

    version :: AlpinePackage -> VerConstraint
    version pkg = CEq $ (alpinePackageArchitecture pkg) <> "#" <> (alpinePackageVersion pkg)

analyze ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  OsInfo ->
  m (Graphing Dependency, GraphBreadth)
analyze _ dbFile osInfo = do
  installed <- context ("Reading alpine database file: " <> toText dbFile) $ readContentsParser installedPackagesDatabaseParser dbFile
  traverse_ warn (lefts installed)
  context "building graphing alpine packages" $ pure (buildGraph osInfo $ rights installed, Complete)
