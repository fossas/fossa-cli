module Types
  ( DiscoverEffs
  , Discover(..)
  , StrategyEffs
  , Strategy(..)
  , StrategyGroup(..)

  , Optimal(..)
  , Complete(..)

  , ConfiguredStrategy(..)
  , SomeStrategy(..)

  , CompletedStrategy(..)
  
  , CompletedLicenseScan(..)

  , LicenseResult(..)
  , License(..)
  , LicenseType(..)

  , BasicDirOpts(..)
  , BasicFileOpts(..)
  ) where

import Prologue

import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.Resource

import DepTypes
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Diagnostics
import Graphing

---------- Discovery

-- | The effects available for use in 'Discover'
type DiscoverEffs r = Members '[Embed IO, Resource, Logger, Error CLIErr, Exec, Error ExecErr, ReadFS, Error ReadFSErr, Output ConfiguredStrategy] r

-- | Discover functions produce 'ConfiguredStrategy's, given a base directory
-- to search
data Discover = Discover
  { discoverName :: Text
  , discoverFunc :: forall r. DiscoverEffs r => Path Abs Dir -> Sem r ()
  }

---------- Strategies

-- FUTURE: determine strategy completeness during scan?

data Optimal = Optimal | NotOptimal
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Optimal where
  toJSON Optimal    = toJSON True
  toJSON NotOptimal = toJSON False

data Complete = Complete | NotComplete
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Complete where
  toJSON Complete    = toJSON True
  toJSON NotComplete = toJSON False

-- | The effects available for use in 'Strategy'
type StrategyEffs r = Members '[Embed IO, Resource, Logger, Error CLIErr, Exec, Error ExecErr, ReadFS, Error ReadFSErr] r

-- | Strategies produce dependency graphs
--
-- @options@ must have 'ToJSON' and 'FromJSON' instances -- these are used to
-- serialize\/deserialize a strategy's options to/from disk
data Strategy options = Strategy
  { strategyName     :: Text -- ^ e.g., "python-pipenv"
  , strategyAnalyze  :: forall r. StrategyEffs r => options -> Sem r (Graphing Dependency)
  , strategyLicense  :: forall r. StrategyEffs r => options -> Sem r [LicenseResult]
  , strategyModule   :: options -> Path Rel Dir -- ^ Determine the module directory for grouping with other strategies
  , strategyOptimal  :: Optimal -- ^ Whether this strategy is considered "optimal" -- i.e., best case analysis for a given project. Notably, this __does not__ imply "complete".
  , strategyComplete :: Complete -- ^ Whether this strategy produces graphs that contain all dependencies for a given project. When @NotComplete@, the backend will run a hasGraph-like analysis to produce the missing dependencies and graph edges
  }

-- | 'Strategy' outputs are grouped and sorted based on the provided @StrategyGroup@s
--
-- For example, @"python"@ is a @StrategyGroup@ that has pipenv, piplist, ... as strategies
data StrategyGroup = StrategyGroup
  { groupName       :: Text -- ^ e.g., "python"
  , groupStrategies :: [SomeStrategy]
  }

-- | An arbitrary 'Strategy', suitable for use in lists, map values, ...
-- Used to construct 'StrategyGroup's
data SomeStrategy where
  SomeStrategy :: Strategy options -> SomeStrategy

---------- Configured Strategies

-- | A strategy paired with its options. Produced by 'Discover' functions
data ConfiguredStrategy where
  ConfiguredStrategy :: Strategy options -> options -> ConfiguredStrategy

---------- Completed Strategies

-- | Completed strategy output. See 'Strategy' for additional information about
-- these fields.
data CompletedStrategy = CompletedStrategy
  { completedName     :: Text
  , completedModule   :: Path Rel Dir
  , completedGraph    :: Graphing Dependency
  , completedOptimal  :: Optimal
  , completedComplete :: Complete
  } deriving (Eq, Ord, Show, Generic)

---------- Basic Opts

-- | A basic set of options, containing just a target directory
newtype BasicDirOpts = BasicDirOpts
  { targetDir :: Path Rel Dir
  } deriving (Eq, Ord, Show, Generic)

-- | A basic set of options, containing just a target file
newtype BasicFileOpts = BasicFileOpts
  { targetFile :: Path Rel File
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON BasicFileOpts where
  parseJSON = withObject "BasicFileOpts" $ \obj ->
    BasicFileOpts <$> obj .: "file"

instance ToJSON BasicFileOpts where
  toJSON BasicFileOpts{..} = object ["file" .= targetFile]

---------- License

data LicenseResult = LicenseResult
  { licenseFile   :: Path Rel File
  , licensesFound :: [License]
  } deriving (Eq, Ord, Show, Generic)

data License = License
  { licenseType :: LicenseType
  , value       :: Text
  } deriving (Eq, Ord, Show, Generic)

data LicenseType =
          LicenseURL
        | LicenseFile
        | LicenseSPDX
        | UnknownType
          deriving (Eq, Ord, Show, Generic)

instance ToJSON License where
    toJSON License{..} = object
      [ "type"   .=  textType licenseType
      , "value"  .=  value
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

---------- Completed License Scan

data CompletedLicenseScan = CompletedLicenseScan
  { completedLicenseName     :: Text
  , completedLicenses        :: [LicenseResult]
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON CompletedLicenseScan where
    toJSON CompletedLicenseScan{..} = object
      [ "name"            .=  completedLicenseName
      , "licenseResults"  .=  completedLicenses
      ]