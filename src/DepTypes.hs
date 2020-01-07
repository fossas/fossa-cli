module DepTypes
  ( Dependency(..)
  , DepType(..)
  , VerConstraint(..)
  ) where

import Prologue

data Dependency = Dependency
  { dependencyType      :: DepType
  , dependencyName      :: Text
  , dependencyVersion   :: Maybe VerConstraint
  , dependencyLocations :: [Text]
  , dependencyTags      :: Map Text [Text]
  } deriving (Eq, Ord, Show, Generic)

-- | A Dependency type. This corresponds to a "fetcher" on the backend
data DepType =
    SubprojectType -- ^ A first-party subproject
  | GemType    -- ^ Gem registry
  | MavenType -- ^ Maven registry
  | NodeJSType -- ^ NPM registry (or similar)
  | NuGetType -- ^ Nuget registry
  | PipType    -- ^ Pip registry
  | GoType -- ^ Go dependency
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
  deriving (Eq, Ord, Show, Generic)

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
      CLessOrEq text -> ("LESSTHANOREQUAL", toJSON text)
      CGreater text -> ("GREATERTHAN", toJSON text)
      CGreaterOrEq text -> ("GREATEROREQUAL", toJSON text)
      CNot text -> ("NOT", toJSON text)
