{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.PackageReference
  ( discover
  , findProjects
  , getDeps
  , mkProject
  , buildGraph

  , PackageReference(..)
  , ItemGroup(..)
  , Package(..)
  ) where

import Control.Applicative (optional)
import Control.Effect.Diagnostics
import Data.Foldable (find)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Text (Text)
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing
import Parse.XML
import Path
import Types

isPackageRefFile :: Path b File -> Bool
isPackageRefFile file = any (\x -> x `L.isSuffixOf` fileName file) [".csproj", ".xproj", ".vbproj", ".dbproj", ".fsproj"]

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = context "PackageReference" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [PackageReferenceProject]
findProjects = walk' $ \_ _ files -> do
  case find isPackageRefFile files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([PackageReferenceProject file], WalkContinue)

newtype PackageReferenceProject = PackageReferenceProject
  { packageReferenceFile :: Path Abs File
  }
  deriving (Eq, Ord, Show)

mkProject :: (Has ReadFS sig n, Has Diagnostics sig n) => PackageReferenceProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "packagereference",
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ getDeps project,
      projectPath = parent $ packageReferenceFile project,
      projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => PackageReferenceProject -> m (Graphing Dependency)
getDeps = context "PackageReference" . context "Static analysis" . analyze' . packageReferenceFile

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = do
  ref <- readContentsXML @PackageReference file
  context "Building dependency graph" $ pure (buildGraph ref)

newtype PackageReference = PackageReference
  { groups :: [ItemGroup]
  } deriving (Eq, Ord, Show)

newtype ItemGroup = ItemGroup
  { dependencies :: [Package]
  } deriving (Eq, Ord, Show)

data Package = Package
  { depID      :: Text
  , depVersion :: Maybe Text
  } deriving (Eq, Ord, Show)

instance FromXML PackageReference where
  parseElement el = PackageReference <$> children "ItemGroup" el

instance FromXML ItemGroup where
  parseElement el = ItemGroup <$> children "PackageReference" el

instance FromXML Package where
  parseElement el =
    Package <$> attr "Include" el
            <*> optional (child "Version" el)

buildGraph :: PackageReference -> Graphing Dependency
buildGraph project = Graphing.fromList (map toDependency direct)
    where
    direct = concatMap dependencies (groups project)
    toDependency Package{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = depID
               , dependencyVersion =  fmap CEq depVersion
               , dependencyLocations = []
               , dependencyEnvironments = []
               , dependencyTags = M.empty
               }
