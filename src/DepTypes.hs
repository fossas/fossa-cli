{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module DepTypes (
  Dependency (..),
  insertEnvironment,
  insertTag,
  insertLocation,
  DepEnvironment (..),
  DepType (..),
  VerConstraint (..),
) where

import Data.Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)

-- FIXME: this needs a smart constructor with empty tags/environments/etc.
-- We've historically relied on the compile error for making sure we fill all
-- of these fields. Tests should be used to ensure this instead.
data Dependency = Dependency
  { dependencyType :: DepType
  , dependencyName :: Text
  , dependencyVersion :: Maybe VerConstraint
  , dependencyLocations :: [Text]
  , dependencyEnvironments :: [DepEnvironment] -- FIXME: this should be a Set
  , dependencyTags :: Map Text [Text]
  }
  deriving (Eq, Ord, Show)

insertEnvironment :: DepEnvironment -> Dependency -> Dependency
insertEnvironment env dep = dep{dependencyEnvironments = env : dependencyEnvironments dep}

insertTag :: Text -> Text -> Dependency -> Dependency
insertTag key value dep = dep{dependencyTags = M.insertWith (++) key [value] (dependencyTags dep)}

insertLocation :: Text -> Dependency -> Dependency
insertLocation loc dep = dep{dependencyLocations = loc : dependencyLocations dep}

data DepEnvironment
  = EnvProduction
  | EnvDevelopment
  | EnvTesting
  | -- | Other environments -- specifically for things like gradle configurations
    EnvOther Text
  deriving (Eq, Ord, Show)

-- | A Dependency type. This corresponds to a "fetcher" on the backend
data DepType
  = -- | A first-party subproject
    SubprojectType
  | -- | Dependency found from the composer fetcher.
    ComposerType
  | -- | Conda dependency
    CondaType
  | -- | Repository in Github
    GitType
  | -- | Gem registry
    GemType
  | -- | android.googlesource.com
    GooglesourceType
  | -- | Hex registry
    HexType
  | -- | Maven registry
    MavenType
  | -- | NPM registry (or similar)
    NodeJSType
  | -- | Nuget registry
    NuGetType
  | -- | Pip registry
    PipType
  | -- | Cocoapods registry
    PodType
  | -- | Go dependency
    GoType
  | -- | Rust Cargo Dependency
    CargoType
  | -- | RPM dependency
    RPMType
  | -- | URL dependency
    URLType
  | -- | No registry or fetch work needed
    UserType
  | -- | Hackage Registry
    -- TODO: does this break the "location" abstraction?
    HackageType
  | -- | A Carthage dependency -- effectively a "git" dependency. Name is repo path and version is tag/branch/hash
    CarthageType
  deriving (Eq, Ord, Show, Generic)

data VerConstraint
  = -- | equal to, e.g., @CEq "2.0.0"@
    CEq Text
  | -- | An exact version, at some URI
    CURI Text
  | -- | compatible range. e.g., "~=" in python, "^>=" in haskell
    CCompatible Text
  | -- | Both constraints need to be satisfied, e.g., @CAnd (CGreaterOrEq "1.0.0") (CLessThan "2.0.0")@
    CAnd VerConstraint VerConstraint
  | -- | At least one constraint needs to be satisfied
    COr VerConstraint VerConstraint
  | -- | less than
    CLess Text
  | -- | less than or equal to
    CLessOrEq Text
  | -- | greater than
    CGreater Text
  | -- | greater than or equal to
    CGreaterOrEq Text
  | -- | not this version
    CNot Text
  deriving (Eq, Ord, Show)

instance FromJSON DepType -- use the generic instance
instance ToJSON DepType -- use the generic instance

instance ToJSON Dependency where
  toJSON Dependency{..} =
    object
      [ "type" .= dependencyType
      , "name" .= dependencyName
      , "version" .= dependencyVersion
      , "locations" .= dependencyLocations
      , "tags" .= dependencyTags
      ]

instance ToJSON VerConstraint where
  toJSON constraint =
    let (name, value) = toValue constraint
     in object
          [ "type" .= name
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
