module Strategy.Dpkg.Dpkg (analyze) where

import Container.OsRelease (OsInfo (OsInfo))
import Control.Effect.Diagnostics (
  Diagnostics,
  context,
 )
import Data.String.Conversion (toText)
import DepTypes (
  DepType (LinuxDEB),
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
import Strategy.Dpkg.Parser (
  DpkgEntry (..),
  dpkgEntriesParser,
 )
import Types (GraphBreadth (..))

buildGraph :: OsInfo -> [DpkgEntry] -> Graphing Dependency
buildGraph (OsInfo os osVersion) pkgs = directs (map toDependency pkgs)
  where
    toDependency :: DpkgEntry -> Dependency
    toDependency pkg =
      Dependency
        LinuxDEB
        (dpkgEntryPackage pkg <> "#" <> os <> "#" <> osVersion)
        (Just $ version pkg)
        mempty
        mempty
        mempty

    version :: DpkgEntry -> VerConstraint
    version pkg = CEq $ (dpkgEntryArch pkg) <> "#" <> (dpkgEntryVersion pkg)

analyze ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  OsInfo ->
  m (Graphing Dependency, GraphBreadth)
analyze _ dbFile osInfo = do
  installed <- context ("Reading dpkg database file: " <> toText dbFile) $ readContentsParser dpkgEntriesParser dbFile
  context "building graphing dpkg packages" $ pure (buildGraph osInfo installed, Complete)
