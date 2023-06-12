module Network.HTTP.Req.Extra (
  httpConfigRetryTimeouts,
) where

import Control.Exception (SomeException, fromException)
import Control.Retry (RetryPolicy, RetryStatus, constantDelay, limitRetriesByCumulativeDelay)
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (ConnectionTimeout, ResponseTimeout), Response (responseStatus))
import Network.HTTP.Req (
  HttpConfig (
    httpConfigRetryJudge,
    httpConfigRetryJudgeException,
    httpConfigRetryPolicy
  ),
  defaultHttpConfig,
 )
import Network.HTTP.Types (statusCode)

-- | An HttpConfig that adds a retry handler for ResponseTimeout
--
-- ResponseTimeout occurs when the backend is available (the edge server accepts
-- our connection) but the pod is not responding to our request
--
-- Some examples of behavior to expect:
--
-- | Status Code | Description                   | Result  |
-- |-------------|-------------------------------|---------|
-- | 500         | Internal Server Failure       | Fails   |
-- | 502         | Bad Gateway                   | Retries |
-- | 503         | Service Unavailable           | Fails   |
-- | 408         | Request Timeout               | Retries |
-- | 504         | Gateway Timeout               | Retries |
-- | 524         | A Timeout Occurred            | Retries |
-- | 598         | Network read timeout error    | Retries |
-- | 599         | Network connect timeout error | Retries |
-- | No Network  | N/A                           | Fails   |
-- -
httpConfigRetryTimeouts :: HttpConfig
httpConfigRetryTimeouts =
  defaultHttpConfig
    { httpConfigRetryPolicy = retryPolicy
    , -- Determines if we should retry a request,
      -- when exception is encountered
      httpConfigRetryJudgeException = isTimeout
    , -- Determines when we should retry a request,
      -- in which did we did not encounter an exception.
      httpConfigRetryJudge = \_ response ->
        statusCodeOf response
          `elem` [
                   -- https://en.wikipedia.org/wiki/List_of_HTTP_status_codes
                   408 -- request timeout
                 , 502 -- bad gateway
                 , 504 -- gateway timeout
                 , 524 -- a timeout occurred
                 , 598 -- network read timeout error
                 , 599 -- network connect timeout error
                 ]
    }
  where
    statusCodeOf = statusCode . responseStatus

-- | Retry every 5 seconds for up to 10 minutes
retryPolicy :: RetryPolicy
retryPolicy = limitRetriesByCumulativeDelay tenMinutes (constantDelay fiveSeconds)
  where
    -- ten minutes
    tenMinutes :: Int
    tenMinutes = 10 * 60 * 1_000_000

    -- five seconds
    fiveSeconds :: Int
    fiveSeconds = 5 * 1_000_000

-- | True if the exception represnts timeout, otherwise False.
isTimeout :: RetryStatus -> SomeException -> Bool
isTimeout _ e =
  case fromException e of
    Just (HttpExceptionRequest _ c) ->
      case c of
        ResponseTimeout -> True
        ConnectionTimeout -> True
        _ -> False
    _ -> False
