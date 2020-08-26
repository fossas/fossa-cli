{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.NuGet.PackageReference
  ( discover
  , buildGraph
  , analyze

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

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find isPackageRefFile files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "nuget-packagereference" DotnetGroup $ analyze file

  pure WalkContinue
 
  where 
      isPackageRefFile :: Path b File -> Bool
      isPackageRefFile file = any (\x -> L.isSuffixOf x (fileName file)) [".csproj", ".xproj", ".vbproj", ".dbproj", ".fsproj"]

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsXML @PackageReference file

mkProjectClosure :: Path Abs File -> PackageReference -> ProjectClosureBody
mkProjectClosure file package = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph package
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

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
