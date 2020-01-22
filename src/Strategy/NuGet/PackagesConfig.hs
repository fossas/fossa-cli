module Strategy.NuGet.PackagesConfig
  ( discover
  , strategy
  , buildGraph
  , analyze
  , parsePackagesConfig

  , NuGetDependency(..)
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output
import qualified Text.XML.Light as XML

import           DepTypes
import           Discovery.Walk
import           Effect.ReadFS
import           Graphing (Graphing, unfold)
import           Types

discover :: Discover
discover = Discover
  { discoverName = "packagesconfig"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (\f -> (fileName f) == "packages.config") files of
    Just file -> output (configure file)
    Nothing -> pure ()

  walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "nuget-packagesconfig"
  , strategyAnalyze = \opts -> analyze & fileInputXML (targetFile opts) parsePackagesConfig
  , strategyModule = parent . targetFile
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

analyze :: Member (Input [NuGetDependency]) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

data NuGetDependency = NuGetDependency
  { depID        :: String
  , depVersion   :: String
  } deriving (Eq, Ord, Show, Generic)

parsePackagesConfig :: XML.Element -> Maybe [NuGetDependency]
parsePackagesConfig packagesConfig = do
  guard (XML.qName (XML.elName packagesConfig) == "packages")
  traverse parsePackage $ childrenByName "package" packagesConfig
  where

  parsePackage :: XML.Element -> Maybe NuGetDependency
  parsePackage el = do
    elID  <- attrByName "id" el
    version <- attrByName "version" el

    pure (NuGetDependency elID version)

  attrByName :: String -> XML.Element -> Maybe String
  attrByName name element = XML.findAttrBy (\elName -> XML.qName elName == name) element 

  childrenByName :: String -> XML.Element -> [XML.Element]
  childrenByName name = XML.filterChildrenName (\elName -> XML.qName elName == name)

buildGraph :: [NuGetDependency] -> Graphing Dependency
buildGraph dependencies = unfold dependencies (const []) toDependency
    where
    toDependency NuGetDependency{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = T.pack depID
               , dependencyVersion = Just (CEq $ T.pack depVersion)
               , dependencyLocations = []
               , dependencyTags = M.empty
               }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts