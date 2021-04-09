{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Types
  ( DiscoveredProject(..)
  , BuildTarget(..)

  , LicenseResult(..)
  , License(..)
  , LicenseType(..)

  , module DepTypes
  ) where

import Data.Aeson
import Data.Set (Set)
import Data.Text (Text)
import DepTypes
import Graphing
import Path

-- TODO: results should be within a graph of build targets && eliminate SubprojectType
-- | A project found during project discovery, parameterized by the monad
-- used to perform dependency analysis
data DiscoveredProject m = DiscoveredProject
  { projectType :: Text,
    projectPath :: Path Abs Dir,
    projectBuildTargets :: Set BuildTarget,
    projectDependencyGraph :: Set BuildTarget -> m (Graphing Dependency),
    projectLicenses :: m [LicenseResult]
  }

newtype BuildTarget = BuildTarget { unBuildTarget :: Text }
  deriving (Eq, Ord, Show)

data LicenseResult = LicenseResult
  { licenseFile   :: FilePath
  , licensesFound :: [License]
  } deriving (Eq, Ord, Show)

data License = License
  { licenseType  :: LicenseType
  , licenseValue :: Text
  } deriving (Eq, Ord, Show)

data LicenseType =
          LicenseURL
        | LicenseFile
        | LicenseSPDX
        | UnknownType
          deriving (Eq, Ord, Show)

instance ToJSON License where
    toJSON License{..} = object
      [ "type"   .=  textType licenseType
      , "value"  .=  licenseValue
      ]

      where
        textType :: LicenseType -> Text
        textType = \case
          LicenseURL  -> "url"
          LicenseFile -> "file"
          LicenseSPDX -> "spdx"
          UnknownType -> "unknown"

instance ToJSON LicenseResult where
    toJSON LicenseResult{..} = object
      [ "filepath"  .=  licenseFile
      , "licenses"  .=  licensesFound
      ]
