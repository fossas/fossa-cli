module App.Fossa.Config.ReleaseGroup (

) where

import App.Fossa.Config.Common (CommonOpts)
import App.Fossa.Subcommand (EffStack, GetCommonOpts, GetSeverity (..), SubCommand (..))
import App.Types (BaseDir)
import Control.Carrier.Lift (sendIO)
import Control.Effect.Diagnostics (Diagnostics, Has)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.Text (Text)
import GHC.Generics (Generic)
import Options.Applicative (InfoMod, Parser, command, eitherReader, helpDoc, info, long, many, metavar, option, optional, progDescDoc, short, strOption, subparser, switch, (<|>))
import Style (applyFossaStyle, formatStringToDoc, stringToHelpDoc)

-- mkSubCommand ::

newtype ReleaseGroupConfig = Create CreateConfig
  deriving (Show, Generic)

-- \| Delete DeleteConfig
-- \| DeleteRelease DeleteReleaseConfig

data CreateConfig = CreateConfig
  { x :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CreateConfig where
  toEncoding = genericToEncoding defaultOptions

data ReleaseGroupCreateOptions = ReleaseGroupCreateOptions
  { commonOpts :: CommonOpts
  , title :: Text
  , release :: Text
  , releaseGroupProjects :: [ReleaseGroupProject]
  , licenseCompliancePolicy :: Maybe Text
  , vulnerabilityManagementPolicy :: Maybe Text
  , teams :: Maybe [Text]
  }
  deriving (Eq, Ord, Show, Generic)

data ReleaseGroupProject = ReleaseGroupProject
  { projectId :: Text
  , projectRevision :: Text
  , projectBranch :: Text
  }
  deriving (Eq, Ord, Show, Generic)
