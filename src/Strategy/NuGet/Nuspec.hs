{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.Nuspec
  ( discover,
    findProjects,
    getDeps,
    mkProject,
    buildGraph,

    Nuspec(..),
    Group(..),
    NuGetDependency(..),
    NuspecLicense(..),
  ) where

import Control.Applicative (optional)
import Control.Effect.Diagnostics
import Data.Foldable (find)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing
import Parse.XML
import Path
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = map mkProject <$> findProjects dir

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [NuspecProject]
findProjects = walk' $ \_ _ files -> do
  case find (L.isSuffixOf ".nuspec" . fileName) files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([NuspecProject file], WalkContinue)

newtype NuspecProject = NuspecProject
  { nuspecFile :: Path Abs File
  }
  deriving (Eq, Ord, Show)

mkProject :: (Has ReadFS sig n, Has Diagnostics sig n) => NuspecProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "nuspec",
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ getDeps project,
      projectPath = parent $ nuspecFile project,
      projectLicenses = analyzeLicenses (nuspecFile project)
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => NuspecProject -> m (Graphing Dependency)
getDeps = analyze' . nuspecFile

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = buildGraph <$> readContentsXML @Nuspec file

analyzeLicenses :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m [LicenseResult]
analyzeLicenses file = do
  nuspec <- readContentsXML @Nuspec file
  pure [LicenseResult (toFilePath file) (nuspecLicenses nuspec)]

nuspecLicenses :: Nuspec -> [License]
nuspecLicenses nuspec = url ++ licenseField
          where
            url = case licenseUrl nuspec of
              Just location -> [License LicenseURL location]
              Nothing -> []
            licenseField = foldr (\a b -> b ++ [License (parseLicenseType $ nuspecLicenseType a) (nuspecLicenseValue a)]) [] (license nuspec)

parseLicenseType :: Text -> LicenseType
parseLicenseType rawType = case T.unpack rawType of
                            "expression" -> LicenseSPDX
                            "file" -> LicenseFile
                            _ -> UnknownType

data Nuspec = Nuspec
  { groups        :: [Group]
  , license       :: [NuspecLicense]
  , licenseUrl    :: Maybe Text
  } deriving (Eq, Ord, Show)

data NuspecLicense = NuspecLicense
  { nuspecLicenseType  :: Text
  , nuspecLicenseValue :: Text
  } deriving (Eq, Ord, Show)

newtype Group = Group
  { dependencies  :: [NuGetDependency]
  } deriving (Eq, Ord, Show)

data NuGetDependency = NuGetDependency
  { depID      :: Text
  , depVersion :: Text
  } deriving (Eq, Ord, Show)

instance FromXML Nuspec where
  parseElement el = do
    metadata     <- child "metadata" el
    Nuspec <$> optional (child "dependencies" metadata >>= children "group") `defaultsTo` []
           <*> children "license" metadata
           <*> optional (child "licenseUrl" metadata)

instance FromXML NuspecLicense where
  parseElement el =
    NuspecLicense <$> optional (attr "type" el) `defaultsTo` ""
                  <*> content el

instance FromXML Group where
  parseElement el = Group <$> children "dependency" el

instance FromXML NuGetDependency where
  parseElement el =
    NuGetDependency <$> attr "id" el
                    <*> attr "version" el

buildGraph :: Nuspec -> Graphing Dependency
buildGraph project = Graphing.fromList (map toDependency direct)
    where
    direct = concatMap dependencies (groups project)
    toDependency NuGetDependency{..} =
      Dependency { dependencyType = NuGetType
                 , dependencyName = depID
                 , dependencyVersion = Just (CEq depVersion)
                 , dependencyLocations = []
                 , dependencyEnvironments = []
                 , dependencyTags = M.empty
                 }
