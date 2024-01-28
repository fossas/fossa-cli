{-# LANGUAGE RecordWildCards #-}

module Strategy.Maven.Pom.PomFile
-- TODO: sort
  (
  Pom (..),
  PomBuild (..),
  RawDependency (..),
  RawParent (..),
  RawPom (..),
  MavenCoordinate (..),
  MvnDepBody (..),
  PomLicense (..),
  Group,
  Artifact,
  validatePom,
) where

import Control.Applicative (optional, (<|>))
import Data.Aeson (ToJSON, ToJSONKey)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Parse.XML

----- Validating POM files

-- TODO: validation errors?
validatePom :: RawPom -> Maybe Pom
validatePom raw = do
  coord <- validateCoordinate raw
  let parentCoord = parentToCoordinate <$> rawPomParent raw
      properties = rawPomProperties raw

      dependencyManagement = rawDepsToDeps (rawPomDependencyManagement raw)
      dependencies = rawDepsToDeps (rawPomDependencies raw)
      licenses = rawPomLicenses raw

      groupOfBuild = coordGroup coord
      artifactOfBuild = coordArtifact coord
      build = Map.singleton (groupOfBuild, artifactOfBuild) (rawPomBuild raw)
  pure (Pom coord parentCoord properties dependencyManagement dependencies licenses build)

rawDepsToDeps :: [RawDependency] -> Map (Group, Artifact) MvnDepBody
rawDepsToDeps = Map.fromList . map (\dep -> (depToKey dep, depToBody dep))
  where
    depToKey :: RawDependency -> (Group, Artifact)
    depToKey raw = (rawDependencyGroup raw, rawDependencyArtifact raw)

    depToBody :: RawDependency -> MvnDepBody
    depToBody RawDependency{..} =
      MvnDepBody
        { depVersion = rawDependencyVersion
        , depClassifier = rawDependencyClassifier
        , depScope = rawDependencyScope
        , depOptional = rawDependencyOptional
        }

-- Build a full coordinate from a pom, falling back to the group/version from
-- the parent
--
-- From the docs:
-- "although, groupId and version need not be explicitly defined if they are inherited from a parent"
-- https://maven.apache.org/pom.html#Maven_Coordinates
validateCoordinate :: RawPom -> Maybe MavenCoordinate
validateCoordinate RawPom{..} = MavenCoordinate <$> group <*> artifact <*> version
  where
    group, artifact, version :: Maybe Text
    group = rawPomGroup <|> (rawParentGroup <$> rawPomParent)
    artifact = pure rawPomArtifact
    version = rawPomVersion <|> (rawParentVersion <$> rawPomParent)

parentToCoordinate :: RawParent -> MavenCoordinate
parentToCoordinate RawParent{..} =
  MavenCoordinate
    { coordGroup = rawParentGroup
    , coordArtifact = rawParentArtifact
    , coordVersion = rawParentVersion
    }

-- TODO: newtypes?
type Group = Text
type Artifact = Text
data Pom = Pom
  { pomCoord :: MavenCoordinate
  , pomParentCoord :: Maybe MavenCoordinate
  , pomProperties :: Map Text Text
  , pomDependencyManagement :: Map (Group, Artifact) MvnDepBody
  , pomDependencies :: Map (Group, Artifact) MvnDepBody
  , pomLicenses :: [PomLicense]
  , pomBuilds :: Map (Group, Artifact) (Maybe PomBuild)
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Pom

data MavenCoordinate = MavenCoordinate
  { coordGroup :: Text
  , coordArtifact :: Text
  , coordVersion :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MavenCoordinate
instance ToJSONKey MavenCoordinate

data MvnDepBody = MvnDepBody
  { depVersion :: Maybe Text
  , depClassifier :: Maybe Text
  , depScope :: Maybe Text
  , depOptional :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MvnDepBody

instance Semigroup Pom where
  -- left-biased, similar to Map.union
  --
  -- almost all fields are inherited -- and the only ones we're tracking are
  -- properties/dependencyManagement/dependencies
  --
  -- we overlay dependencyManagement and dependencies with MvnDepBody's
  -- Semigroup instance
  childPom <> parentPom =
    Pom
      { pomCoord = pomCoord childPom
      , pomParentCoord = pomParentCoord childPom
      , pomProperties = Map.union (pomProperties childPom) (pomProperties parentPom)
      , pomDependencyManagement = Map.unionWith (<>) (pomDependencyManagement childPom) (pomDependencyManagement parentPom)
      , pomDependencies = Map.unionWith (<>) (pomDependencies childPom) (pomDependencies parentPom)
      , pomLicenses = pomLicenses childPom
      , pomBuilds = Map.union (pomBuilds childPom) (pomBuilds parentPom)
      }

instance Semigroup MvnDepBody where
  -- left-biased, similar to Map.union
  left <> right =
    MvnDepBody
      { depVersion = depVersion left <|> depVersion right
      , depClassifier = depClassifier left <|> depClassifier right
      , depScope = depScope left <|> depScope right
      , depOptional = depOptional left <|> depOptional right
      }

----- Raw POM files + deserialization

-- a POM file as found in the filesystem
data RawPom = RawPom
  { rawPomParent :: Maybe RawParent
  , rawPomGroup :: Maybe Text
  , rawPomArtifact :: Text
  , rawPomVersion :: Maybe Text
  , rawPomName :: Maybe Text
  , rawPomProperties :: Map Text Text
  , rawPomModules :: [Text]
  , rawPomBuild :: Maybe PomBuild
  , rawPomDependencyManagement :: [RawDependency]
  , rawPomDependencies :: [RawDependency]
  , rawPomLicenses :: [PomLicense]
  }
  deriving (Eq, Ord, Show)

data RawParent = RawParent
  { rawParentGroup :: Text
  , rawParentArtifact :: Text
  , rawParentVersion :: Text
  , rawParentRelativePath :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data RawDependency = RawDependency
  { rawDependencyGroup :: Text
  , rawDependencyArtifact :: Text
  , rawDependencyVersion :: Maybe Text
  , rawDependencyClassifier :: Maybe Text
  , rawDependencyScope :: Maybe Text
  , rawDependencyOptional :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data PomLicense = PomLicense
  { pomLicenseName :: Maybe Text
  , pomLicenseUrl :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PomLicense

-- | Ref: https://maven.apache.org/pom.html#build
data PomBuild = PomBuild
  { -- This is the name of the bundled project when it is finally built
    -- (sans the file extension, for example: my-project-1.0.jar)
    pomBuildFinalName :: Maybe Text
  , -- This is the directory where the build will dump its files
    pomBuildOutputDirectory :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PomBuild
instance FromXML RawPom where
  parseElement el =
    RawPom
      <$> optional (child "parent" el)
      <*> optional (child "groupId" el)
      <*> child "artifactId" el
      <*> optional (child "version" el)
      <*> optional (child "name" el)
      <*> optional (child "properties" el)
        `defaultsTo` Map.empty
      <*> optional (child "modules" el >>= children "module")
        `defaultsTo` []
      <*> optional (child "build" el)
      <*> optional (child "dependencyManagement" el >>= children "dependency")
        `defaultsTo` []
      <*> optional (child "dependencies" el >>= children "dependency")
        `defaultsTo` []
      <*> optional (child "licenses" el >>= children "license")
        `defaultsTo` []

instance FromXML RawParent where
  -- TODO: move this documentation
  -- "Notice the relativePath element. It is not required, but may be used as a signifier to Maven to first
  -- search the path given for this project's parent, before searching the local and then remote repositories."
  -- https://maven.apache.org/pom.html#Inheritance
  parseElement el =
    RawParent
      <$> child "groupId" el
      <*> child "artifactId" el
      <*> child "version" el
      <*> optional (child "relativePath" el)

instance FromXML RawDependency where
  parseElement el =
    RawDependency
      <$> child "groupId" el
      <*> child "artifactId" el
      <*> optional (child "version" el)
      <*> optional (child "classifier" el)
      <*> optional (child "scope" el)
      <*> optional (child "optional" el)

instance FromXML PomLicense where
  parseElement el =
    PomLicense
      <$> optional (child "name" el)
      <*> optional (child "url" el)

instance FromXML PomBuild where
  parseElement el =
    PomBuild
      <$> optional (child "finalName" el)
      <*> ( optional
              ( child "outputDirectory" el
                  <|> child "directory" el
              )
          )
