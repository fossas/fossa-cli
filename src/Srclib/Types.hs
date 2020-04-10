module Srclib.Types
  ( SourceUnit(..)
  , SourceUnitType(..)
  , SourceUnitBuild(..)
  , SourceUnitDependency(..)
  , Locator(..)
  , renderLocator
  ) where

import Prologue

data SourceUnit = SourceUnit
  { sourceUnitName :: Text
  , sourceUnitType :: SourceUnitType
  , sourceUnitManifest :: Text -- ^ path to manifest file
  , sourceUnitBuild :: SourceUnitBuild
  } deriving (Eq, Ord, Show, Generic)

data SourceUnitType = SourceUnitTypeDummyCLI
  deriving (Eq, Ord, Show, Generic)

data SourceUnitBuild = SourceUnitBuild
  { buildArtifact :: Text -- ^ always "default"
  , buildSucceeded :: Bool -- ^ always true
  , buildImports :: [Locator]
  , buildDependencies :: [SourceUnitDependency]
  } deriving (Eq, Ord, Show, Generic)

data SourceUnitDependency = SourceUnitDependency
  { sourceDepLocator :: Locator
  , sourceDepImports :: [Locator] -- omitempty
  -- , sourceDepData :: Aeson.Value
  } deriving (Eq, Ord, Show, Generic)

data Locator = Locator
  { locatorFetcher :: Text
  , locatorProject :: Text
  , locatorRevision :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)


-- TODO: "NormalizeGitURL", if/when we get git locators
renderLocator :: Locator -> Text
renderLocator Locator{..} =
  locatorFetcher <> "+" <> locatorProject <> "$" <> fromMaybe "" locatorRevision

instance ToJSON SourceUnit where
  toJSON SourceUnit{..} = object
    [ "Name" .= sourceUnitName
    , "Type" .= sourceUnitType
    , "Manifest" .= sourceUnitManifest
    , "Build" .= sourceUnitBuild
    ]

instance ToJSON SourceUnitBuild where
  toJSON SourceUnitBuild{..} = object
    [ "Artifact" .= buildArtifact
    , "Succeeded" .= buildSucceeded
    , "Imports" .= buildImports
    , "Dependencies" .= buildDependencies
    ]

instance ToJSON SourceUnitDependency where
  toJSON SourceUnitDependency{..} = object
    [ "locator" .= sourceDepLocator
    , "imports" .= sourceDepImports
    ]

instance ToJSON SourceUnitType where
  toJSON SourceUnitTypeDummyCLI = "dummy-cli"

instance ToJSON Locator where
  -- render as text
  toJSON = toJSON . renderLocator
