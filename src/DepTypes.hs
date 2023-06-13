{-# LANGUAGE RecordWildCards #-}

module DepTypes (
  Dependency (..),
  insertEnvironment,
  insertTag,
  insertLocation,
  hydrateDepEnvs,
  DepEnvironment (..),
  DepType (..),
  VerConstraint (..),
) where

import Data.Aeson (
  FromJSON,
  KeyValue ((.=)),
  ToJSON (toJSON),
  ToJSONKey,
  Value,
  object,
 )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Graphing.Hydrate (hydrate)

-- FIXME: this needs a smart constructor with empty tags/environments/etc.
-- We've historically relied on the compile error for making sure we fill all
-- of these fields. Tests should be used to ensure this instead.
data Dependency = Dependency
  { dependencyType :: DepType
  , dependencyName :: Text
  , dependencyVersion :: Maybe VerConstraint
  , dependencyLocations :: [Text]
  , dependencyEnvironments :: Set DepEnvironment
  , dependencyTags :: Map Text [Text]
  }
  deriving (Eq, Ord, Show)

insertEnvironment :: DepEnvironment -> Dependency -> Dependency
insertEnvironment env dep = dep{dependencyEnvironments = env `Set.insert` dependencyEnvironments dep}

insertTag :: Text -> Text -> Dependency -> Dependency
insertTag key value dep = dep{dependencyTags = Map.insertWith (++) key [value] (dependencyTags dep)}

insertLocation :: Text -> Dependency -> Dependency
insertLocation loc dep = dep{dependencyLocations = loc : dependencyLocations dep}

data DepEnvironment
  = EnvProduction
  | EnvDevelopment
  | EnvTesting
  | -- | Other environments -- specifically for things like gradle configurations
    EnvOther Text
  deriving (Eq, Ord, Show)

hydrateDepEnvs :: Graphing Dependency -> Graphing Dependency
hydrateDepEnvs = hydrate dependencyEnvironments $ \envs dep ->
  dep{dependencyEnvironments = envs}

-- | A Dependency type. This corresponds to a "fetcher" on the backend
data DepType
  = -- | An archive upload dependency.
    ArchiveType
  | -- | Bower dependency
    BowerType
  | -- | A first-party subproject
    SubprojectType
  | -- | Dependency found from the conan fetcher.
    ConanType
  | -- | Dependency found from the composer fetcher.
    ComposerType
  | -- | Conda dependency
    CondaType
  | -- | CPAN dependency
    CpanType
  | -- | CRAN dependency
    CranType
  | -- | Custom dependency
    CustomType
  | -- | Repository in Github
    GitType
  | -- | Gem registry
    GemType
  | -- | android.googlesource.com
    GooglesourceType
  | -- | Hex registry
    HexType
  | -- | Linux "apk" registry
    LinuxAPK
  | -- | Linux "debian" registry
    LinuxDEB
  | -- | Linux "rpm" registry
    LinuxRPM
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
  | -- | Pub dependency for dart
    PubType
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
  | -- | A Swift Package Dependency -- effectively a "git" dependency. Name is repo path and version is tag/branch/hash
    SwiftType
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

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
instance ToJSONKey DepType

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
