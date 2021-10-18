{-# LANGUAGE RecordWildCards #-}

module Srclib.Types (
  SourceUnit (..),
  SourceUnitBuild (..),
  SourceUnitDependency (..),
  AdditionalDepData (..),
  SourceUserDefDep (..),
  SourceRemoteDep (..),
  Locator (..),
  renderLocator,
  parseLocator,
) where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Path (File, SomeBase)
import Types (GraphBreadth (..))

data SourceUnit = SourceUnit
  { sourceUnitName :: Text
  , sourceUnitType :: Text
  , -- | path to manifest file
    sourceUnitManifest :: Text
  , sourceUnitBuild :: Maybe SourceUnitBuild
  , sourceUnitGraphBreadth :: GraphBreadth
  , sourceUnitOriginPaths :: [SomeBase File]
  , additionalData :: Maybe AdditionalDepData
  }
  deriving (Eq, Ord, Show)

data SourceUnitBuild = SourceUnitBuild
  { -- | always "default"
    buildArtifact :: Text
  , -- | always true
    buildSucceeded :: Bool
  , buildImports :: [Locator]
  , buildDependencies :: [SourceUnitDependency]
  }
  deriving (Eq, Ord, Show)

data SourceUnitDependency = SourceUnitDependency
  { sourceDepLocator :: Locator
  , sourceDepImports :: [Locator] -- omitempty
  -- , sourceDepData :: Aeson.Value
  }
  deriving (Eq, Ord, Show)

data AdditionalDepData = AdditionalDepData
  { userDefinedDeps :: Maybe [SourceUserDefDep]
  , remoteDeps :: Maybe [SourceRemoteDep]
  }
  deriving (Eq, Ord, Show)

data SourceUserDefDep = SourceUserDefDep
  { srcUserDepName :: Text
  , srcUserDepVersion :: Text
  , srcUserDepLicense :: Text
  , srcUserDepDescription :: Maybe Text
  , srcUserDepHomepage :: Maybe Text
  , srcUserDepOrigin :: Maybe (SomeBase File)
  }
  deriving (Eq, Ord, Show)

data SourceRemoteDep = SourceRemoteDep
  { srcRemoteDepName :: Text
  , srcRemoteDepVersion :: Text
  , srcRemoteDepUrl :: Text
  , srcRemoteDepDescription :: Maybe Text
  , srcRemoteDepHomepage :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data Locator = Locator
  { locatorFetcher :: Text
  , locatorProject :: Text
  , locatorRevision :: Maybe Text
  }
  deriving (Eq, Ord, Show)

renderLocator :: Locator -> Text
renderLocator Locator{..} =
  locatorFetcher <> "+" <> locatorProject <> "$" <> fromMaybe "" locatorRevision

parseLocator :: Text -> Locator
parseLocator raw = Locator fetcher project (if Text.null revision then Nothing else Just revision)
  where
    (fetcher, xs) = Text.breakOn "+" raw
    (project, xs') = Text.breakOn "$" (Text.drop 1 xs)
    revision = Text.drop 1 xs'

instance ToJSON SourceUnit where
  toJSON SourceUnit{..} =
    object
      [ "Name" .= sourceUnitName
      , "Type" .= sourceUnitType
      , "Manifest" .= sourceUnitManifest
      , "Build" .= sourceUnitBuild
      , "GraphBreadth" .= sourceUnitGraphBreadth
      , "OriginPaths" .= sourceUnitOriginPaths
      , "AdditionalDependencyData" .= additionalData
      ]

instance ToJSON SourceUnitBuild where
  toJSON SourceUnitBuild{..} =
    object
      [ "Artifact" .= buildArtifact
      , "Succeeded" .= buildSucceeded
      , "Imports" .= buildImports
      , "Dependencies" .= buildDependencies
      ]

instance ToJSON SourceUnitDependency where
  toJSON SourceUnitDependency{..} =
    object
      [ "locator" .= sourceDepLocator
      , "imports" .= sourceDepImports
      ]

instance ToJSON AdditionalDepData where
  toJSON AdditionalDepData{..} =
    object
      [ "UserDefinedDependencies" .= userDefinedDeps
      , "RemoteDependencies" .= remoteDeps
      ]

instance ToJSON SourceUserDefDep where
  toJSON SourceUserDefDep{..} =
    object
      [ "Name" .= srcUserDepName
      , "Version" .= srcUserDepVersion
      , "License" .= srcUserDepLicense
      , "Description" .= srcUserDepDescription
      , "Homepage" .= srcUserDepHomepage
      , "Origin" .= fmap toText srcUserDepOrigin
      ]

instance ToJSON SourceRemoteDep where
  toJSON SourceRemoteDep{..} =
    object
      [ "Name" .= srcRemoteDepName
      , "Version" .= srcRemoteDepVersion
      , "Url" .= srcRemoteDepUrl
      , "Description" .= srcRemoteDepDescription
      , "Homepage" .= srcRemoteDepHomepage
      ]

instance ToJSON Locator where
  -- render as text
  toJSON = toJSON . renderLocator
