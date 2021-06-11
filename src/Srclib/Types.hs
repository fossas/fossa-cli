{-# LANGUAGE RecordWildCards #-}

module Srclib.Types (
  SourceUnit (..),
  SourceUnitBuild (..),
  SourceUnitDependency (..),
  AdditionalDepData (..),
  SourceUserDefDep (..),
  Locator (..),
  renderLocator,
  parseLocator,
) where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data SourceUnit = SourceUnit
  { sourceUnitName :: Text
  , sourceUnitType :: Text
  , -- | path to manifest file
    sourceUnitManifest :: Text
  , sourceUnitBuild :: Maybe SourceUnitBuild
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

newtype AdditionalDepData = AdditionalDepData
  {userDefinedDeps :: [SourceUserDefDep]}
  deriving (Eq, Ord, Show)

data SourceUserDefDep = SourceUserDefDep
  { srcUserDepName :: Text
  , srcUserDepVersion :: Text
  , srcUserDepLicense :: Text
  , srcUserDepDescription :: Maybe Text
  , srcUserDepUrl :: Maybe Text
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
parseLocator raw = Locator fetcher project (if T.null revision then Nothing else Just revision)
  where
    (fetcher, xs) = T.breakOn "+" raw
    (project, xs') = T.breakOn "$" (T.drop 1 xs)
    revision = T.drop 1 xs'

instance ToJSON SourceUnit where
  toJSON SourceUnit{..} =
    object
      [ "Name" .= sourceUnitName
      , "Type" .= sourceUnitType
      , "Manifest" .= sourceUnitManifest
      , "Build" .= sourceUnitBuild
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
  toJSON (AdditionalDepData userdeps) =
    object
      ["UserDefinedDependencies" .= userdeps]

instance ToJSON SourceUserDefDep where
  toJSON SourceUserDefDep{..} =
    object
      [ "Name" .= srcUserDepName
      , "Version" .= srcUserDepVersion
      , "License" .= srcUserDepLicense
      , "Description" .= srcUserDepDescription
      , "Url" .= srcUserDepUrl
      ]

instance ToJSON Locator where
  -- render as text
  toJSON = toJSON . renderLocator
