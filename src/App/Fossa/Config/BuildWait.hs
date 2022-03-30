module App.Fossa.Config.BuildWait (
  WaitConfig (..),
  defaultWaitConfig,
  defaultApiPollDelay,
) where
   
import Control.Timeout (Duration (Seconds))

newtype WaitConfig = WaitConfig
  { apiPollDelay :: Duration
  }
  deriving (Show, Ord, Eq)

defaultWaitConfig :: WaitConfig
defaultWaitConfig = WaitConfig defaultApiPollDelay

defaultApiPollDelay :: Duration
defaultApiPollDelay = Seconds 8
