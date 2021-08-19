module Network.HTTP.Req.Extra (
  httpConfigRetryTimeouts,
) where

import Control.Exception (SomeException, fromException)
import Control.Retry (RetryPolicy, constantDelay, limitRetriesByCumulativeDelay)
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (ResponseTimeout))
import Network.HTTP.Req (HttpConfig, HttpException (VanillaHttpException), defaultHttpConfig, httpConfigRetryJudgeException, httpConfigRetryPolicy)

-- | An HttpConfig that adds a retry handler for ResponseTimeout
--
-- ResponseTimeout occurs when the backend is available (the edge server accepts
-- our connection) but the pod is not responding to our request
--
-- Some examples of behavior to expect:
--
-- 408 - Request Timeout - Retried
-- 504 - Gateway Timeout - Retried
-- 500 - Internal Server Failure - Immediate failure
-- 502 - Bad Gateway - Immediate failure
-- 503 - Service Unavailable - Immediate failure
-- No internet - Immedate failure
httpConfigRetryTimeouts :: HttpConfig
httpConfigRetryTimeouts =
  defaultHttpConfig
    { httpConfigRetryJudgeException = const isResponseTimeout
    , httpConfigRetryPolicy = retryPolicy
    }

-- | Retry every 5 seconds for up to 5 minutes
retryPolicy :: RetryPolicy
retryPolicy = limitRetriesByCumulativeDelay fiveMinutes (constantDelay fiveSeconds)
  where
    -- five minutes
    fiveMinutes :: Int
    fiveMinutes = 5 * 60 * 1_000_000

    -- five seconds
    fiveSeconds :: Int
    fiveSeconds = 5 * 1_000_000

-- | Is the Exception a ResponseTimeout?
isResponseTimeout :: SomeException -> Bool
isResponseTimeout exc =
  case fromException exc of
    Just (VanillaHttpException (HttpExceptionRequest _ ResponseTimeout)) -> True
    _ -> False
