{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module DepTypes
  ( Dependency(..)
  , insertEnvironment
  , insertTag
  , insertLocation
  , DepEnvironment(..)
  , DepType(..)
  , VerConstraint(..)
  ) where

import Data.Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)

-- FIXME: this needs a smart constructor with empty tags/environments/etc.
-- We've historically relied on the compile error for making sure we fill all
-- of these fields. Tests should be used to ensure this instead.
data Dependency = Dependency
  { dependencyType         :: DepType
  , dependencyName         :: Text
  , dependencyVersion      :: Maybe VerConstraint
  , dependencyLocations    :: [Text]
  , dependencyEnvironments :: [DepEnvironment] -- FIXME: this should be a Set
  , dependencyTags         :: Map Text [Text]
  } deriving (Eq, Ord, Show)

insertEnvironment :: DepEnvironment -> Dependency -> Dependency
insertEnvironment env dep = dep { dependencyEnvironments = env : dependencyEnvironments dep }

insertTag :: Text -> Text -> Dependency -> Dependency
insertTag key value dep = dep { dependencyTags = M.insertWith (++) key [value] (dependencyTags dep) }

insertLocation :: Text -> Dependency -> Dependency
insertLocation loc dep = dep { dependencyLocations = loc : dependencyLocations dep }

data DepEnvironment =
    EnvProduction
  | EnvDevelopment
  | EnvTesting
  | EnvOther Text -- ^ Other environments -- specifically for things like gradle configurations
  deriving (Eq, Ord, Show)

-- | A Dependency type. This corresponds to a "fetcher" on the backend
data DepType =
    SubprojectType -- ^ A first-party subproject
  | ComposerType -- ^ Dependency found from the composer fetcher.  
  | GitType -- ^ Repository in Github
  | GemType    -- ^ Gem registry
  | GooglesourceType  -- ^ android.googlesource.com
  | HexType    -- ^ Hex registry
  | MavenType -- ^ Maven registry
  | NodeJSType -- ^ NPM registry (or similar)
  | NuGetType -- ^ Nuget registry
  | PipType    -- ^ Pip registry
  | PodType    -- ^ Cocoapods registry
  | GoType -- ^ Go dependency
  | CargoType -- ^ Rust Cargo Dependency
  | RPMType -- ^ RPM dependency
  | HackageType -- ^ Hackage Registry
  -- TODO: does this break the "location" abstraction?
  | CarthageType -- ^ A Carthage dependency -- effectively a "git" dependency. Name is repo path and version is tag/branch/hash
  deriving (Eq, Ord, Show, Generic)

data VerConstraint =
    CEq Text -- ^ equal to, e.g., @CEq "2.0.0"@
  | CURI Text -- ^ An exact version, at some URI
  | CCompatible Text -- ^ compatible range. e.g., "~=" in python, "^>=" in haskell
  | CAnd VerConstraint VerConstraint -- ^ Both constraints need to be satisfied, e.g., @CAnd (CGreaterOrEq "1.0.0") (CLessThan "2.0.0")@
  | COr  VerConstraint VerConstraint -- ^ At least one constraint needs to be satisfied
  | CLess Text -- ^ less than
  | CLessOrEq Text -- ^ less than or equal to
  | CGreater Text -- ^ greater than
  | CGreaterOrEq Text -- ^ greater than or equal to
  | CNot Text -- ^ not this version
  deriving (Eq, Ord, Show)

instance FromJSON DepType -- use the generic instance
instance ToJSON DepType -- use the generic instance

instance ToJSON Dependency where
  toJSON Dependency{..} = object
    [ "type"      .= dependencyType
    , "name"      .= dependencyName
    , "version"   .= dependencyVersion
    , "locations" .= dependencyLocations
    , "tags"      .= dependencyTags
    ]

instance ToJSON VerConstraint where
  toJSON constraint = let (name, value) = toValue constraint
                       in object [ "type"  .= name
                                 , "value" .= value
                                 ]

    where

    toValue :: VerConstraint -> (Text, Value)
    toValue = \case
      CEq text -> ("EQUAL", toJSON text)
      CURI text -> ("URI", toJSON text)
      CCompatible text -> ("COMPATIBLE", toJSON text)
      CAnd a b -> ("AND", toJSON [toJSON a, toJSON b])
      COr a b -> ("OR", toJSON [toJSON a, toJSON b])
      CLess text -> ("LESSTHAN", toJSON text)
      CLessOrEq text -> ("LESSOREQUAL", toJSON text)
      CGreater text -> ("GREATERTHAN", toJSON text)
      CGreaterOrEq text -> ("GREATEROREQUAL", toJSON text)
      CNot text -> ("NOT", toJSON text)
