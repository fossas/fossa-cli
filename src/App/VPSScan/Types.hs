module App.VPSScan.Types
  ( VPSOpts (..),
    SherlockOpts (..),
    ScotlandYardOpts (..),
  )
where

import qualified App.VPSScan.Scan.RunIPR as RunIPR
import Prologue
import Text.URI (URI)

data ScotlandYardOpts = ScotlandYardOpts
  {scotlandYardUrl :: URI}
  deriving (Generic)

data SherlockOpts = SherlockOpts
  { sherlockCmdPath :: String,
    sherlockUrl :: String,
    sherlockClientToken :: String,
    sherlockClientID :: String
  }
  deriving (Eq, Ord, Show, Generic)

data VPSOpts = VPSOpts
  { vpsSherlock :: SherlockOpts,
    vpsIpr :: Maybe RunIPR.IPROpts,
    vpsScotlandYard :: ScotlandYardOpts,
    organizationID :: Int,
    projectID :: Text,
    revisionID :: Text
  }
  deriving (Generic)
