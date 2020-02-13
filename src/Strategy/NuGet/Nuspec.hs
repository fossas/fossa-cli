module Strategy.NuGet.Nuspec
  ( discover
  , strategy
  , buildGraph
  , analyze

  , Nuspec(..)
  , Group(..)
  , NuGetDependency(..)
  , NuspecLicense(..)
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Text as T
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
  { discoverName = "nuspec"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (L.isSuffixOf ".nuspec" . fileName) files of
    Just file -> output (configure file)
    Nothing -> pure ()

  walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "nuget-nuspec"
  , strategyAnalyze = \opts -> analyze & fileInputXML @Nuspec (targetFile opts)
  , strategyLicense = \opts -> findLicenses (targetFile opts) & fileInputXML @Nuspec (targetFile opts)
  , strategyModule = parent . targetFile
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

findLicenses :: Member (Input Nuspec) r => Path Rel File -> Sem r [LicenseResult]
findLicenses file = do
  nuspec :: Nuspec <- input
  pure [LicenseResult file (nuspecLicenses nuspec)]

nuspecLicenses :: Nuspec -> [License]
nuspecLicenses nuspec = url ++ licenseField
          where
            url = case licenseUrl nuspec of
              Just location -> [License LicenseURL location]
              Nothing -> []
            licenseField = case license nuspec of
              Just field -> [License (parseLicenseType $ typeField field) (licenseValue field)]
              Nothing -> []

parseLicenseType :: Text -> LicenseType
parseLicenseType rawType = case T.unpack rawType of 
                            "expression" -> LicenseSPDX
                            "file" -> LicenseFile
                            _ -> UnknownType

analyze :: Member (Input Nuspec) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

data Nuspec = Nuspec
  { groups        :: [Group]
  , license       :: Maybe NuspecLicense
  , licenseUrl    :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

data NuspecLicense = NuspecLicense
  { typeField    :: Text
  , licenseValue :: Text
  } deriving (Eq, Ord, Show, Generic)

newtype Group = Group
  { dependencies  :: [NuGetDependency]
  } deriving (Eq, Ord, Show, Generic)

data NuGetDependency = NuGetDependency
  { depID      :: Text
  , depVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromXML Nuspec where
  parseElement el = do
    metadata     <- child "metadata" el
    Nuspec <$> optional (child "dependencies" metadata >>= children "group") `defaultsTo` []
           <*> optional (child "license" metadata)
           <*> optional (child "licenseUrl" metadata)

instance FromXML NuspecLicense where
  parseElement el =
    NuspecLicense <$> attr "type" el
                  <*> content el

instance FromXML Group where
  parseElement el = Group <$> (children "dependency" el)

instance FromXML NuGetDependency where
  parseElement el =
    NuGetDependency <$> attr "id" el
                    <*> attr "version" el

buildGraph :: Nuspec -> Graphing Dependency
buildGraph project = unfold direct (const []) toDependency
    where
    direct = concatMap dependencies (groups project)
    toDependency NuGetDependency{..} =
      Dependency { dependencyType = NuGetType
                 , dependencyName = depID
                 , dependencyVersion = Just (CEq depVersion)
                 , dependencyLocations = []
                 , dependencyTags = M.empty
                 }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
