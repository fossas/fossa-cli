module Strategy.NuGet.Nuspec
  ( discover
  , buildGraph
  , analyze

  , Nuspec(..)
  , Group(..)
  , NuGetDependency(..)
  , NuspecLicense(..)

  , mkProjectClosure
  ) where

import Prologue

import Control.Carrier.Error.Either
import qualified Data.Map.Strict as M
import qualified Data.List as L

import DepTypes
import qualified Data.Text as T
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing
import Parse.XML
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> L.isSuffixOf ".nuspec" (fileName f)) files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "nuget-nuspec" DotnetGroup $ analyze file

  walkContinue

analyze :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Rel File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsXML @Nuspec file

mkProjectClosure :: Path Rel File -> Nuspec -> ProjectClosureBody
mkProjectClosure file nuspec = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = [LicenseResult file (nuspecLicenses nuspec)]
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph nuspec
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

nuspecLicenses :: Nuspec -> [License]
nuspecLicenses nuspec = url ++ licenseField
          where
            url = case licenseUrl nuspec of
              Just location -> [License LicenseURL location]
              Nothing -> []
            licenseField = foldr (\a b -> b ++ [License (parseLicenseType $ typeField a) (licenseValue a)]) [] (license nuspec)

parseLicenseType :: Text -> LicenseType
parseLicenseType rawType = case T.unpack rawType of 
                            "expression" -> LicenseSPDX
                            "file" -> LicenseFile
                            _ -> UnknownType

data Nuspec = Nuspec
  { groups        :: [Group]
  , license       :: [NuspecLicense]
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
           <*> children "license" metadata
           <*> optional (child "licenseUrl" metadata)

instance FromXML NuspecLicense where
  parseElement el =
    NuspecLicense <$> optional (attr "type" el) `defaultsTo` ""
                  <*> content el

instance FromXML Group where
  parseElement el = Group <$> (children "dependency" el)

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
