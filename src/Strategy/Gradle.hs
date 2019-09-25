
module Strategy.Gradle
  (
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

data GradleOpts = GradleOpts
  { gradleOptsCmd               :: Text
  , gradleOptsTask              :: Text
  , gradleOptsOnline            :: Bool
  , gradleOptsAllSubmodules     :: Bool
  , gradleOptsAllConfigurations :: Bool
  , gradleOptsTimeout           :: Text -- TODO: Duration
  , gradleOptsRetries           :: Int
  , gradleOptsProject           :: Text
  , gradleOptsConfiguration     :: Text
  } deriving (Show, Generic)

