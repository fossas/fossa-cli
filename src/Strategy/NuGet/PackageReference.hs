module Strategy.NuGet.PackageReference
  ( discover
  , strategy
  , buildGraph
  , analyze

  , PackageReference(..)
  , ItemGroup(..)
  , Package(..)
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.List as L
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output

import           DepTypes
import           Discovery.Walk
import           Effect.ReadFS
import           Graphing (Graphing, unfold)
import           Parse.XML
import           Types

discover :: Discover
discover = Discover
  { discoverName = "packagereference"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find isPackageRefFile files of
    Just file -> output (configure file)
    Nothing -> pure ()

  walkContinue
 
  where 
      isPackageRefFile :: Path Rel File -> Bool
      isPackageRefFile file = any (\x -> L.isSuffixOf x (fileName file)) [".csproj", ".xproj", ".vbproj", ".dbproj", ".fsproj"]

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "nuget-packagereference"
  , strategyAnalyze = \opts -> analyze & fileInputXML @PackageReference (targetFile opts)
  , strategyLicense = const (pure [])
  , strategyModule = parent . targetFile
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

analyze :: Member (Input PackageReference) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

newtype PackageReference = PackageReference
  { groups :: [ItemGroup]
  } deriving (Eq, Ord, Show, Generic)

newtype ItemGroup = ItemGroup
  { dependencies :: [Package]
  } deriving (Eq, Ord, Show, Generic)

data Package = Package
  { depID      :: Text
  , depVersion :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromXML PackageReference where
  parseElement el = PackageReference <$> children "ItemGroup" el

instance FromXML ItemGroup where
  parseElement el = ItemGroup <$> children "PackageReference" el

instance FromXML Package where
  parseElement el =
    Package <$> attr "Include" el
            <*> optional (child "Version" el)

buildGraph :: PackageReference -> Graphing Dependency
buildGraph project = unfold direct (const []) toDependency
    where
    direct = concatMap dependencies (groups project)
    toDependency Package{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = depID
               , dependencyVersion =  fmap CEq depVersion
               , dependencyLocations = []
               , dependencyTags = M.empty
               }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
