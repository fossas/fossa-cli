module App.VPSScan.Scan.RunSherlock
  ( SherlockOpts(..)
  , Sherlock(..)
  , SherlockC(..)
  , SherlockError(..)
  , execSherlock
  ) where
import Prologue

import Control.Carrier.Error.Either
import Effect.Exec
import qualified Data.Text as T

data SherlockOpts = SherlockOpts
  { sherlockCmdPath :: String
  , sherlockUrl :: String
  , sherlockClientToken :: String
  , sherlockClientID :: String
  } deriving (Eq, Ord, Show, Generic)

sherlockCmdArgs :: String -> SherlockOpts -> [String]
sherlockCmdArgs scanId SherlockOpts{..} = [ "--scan-id", scanId
                                          , "--sherlock-client-token", sherlockClientToken
                                          , "--sherlock-client-id", sherlockClientID
                                          ]

----- sherlock effect

data SherlockError
  = SherlockCommandFailed Text
  deriving (Eq, Ord, Show, Generic)

data Sherlock m k
  = ExecSherlock (Path Abs Dir) Text SherlockOpts (Either SherlockError () -> m k)
  deriving Generic1

execSherlock :: Has Sherlock sig m => Path Abs Dir -> Text -> SherlockOpts -> m (Either SherlockError ())
execSherlock basedir scanId opts = send (ExecSherlock basedir scanId opts pure)

instance HFunctor Sherlock
instance Effect Sherlock

----- production sherlock interpreter

newtype SherlockC m a = SherlockC { runSherlock :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m, Effect sig) => Algebra (Sherlock :+: sig) (SherlockC m) where
  alg (R other) = SherlockC (alg (handleCoercible other))
  alg (L (ExecSherlock basedir scanId opts@SherlockOpts{..} k)) = (k =<<) . SherlockC $ do
    let sherlockCommand :: Command
        sherlockCommand = Command [sherlockCmdPath] [] Never
    result <- runExecIO $ exec basedir sherlockCommand $ sherlockCmdArgs (T.unpack scanId) opts
    case result of
      Left err -> pure (Left (SherlockCommandFailed (T.pack (show err))))
      Right _ -> pure (Right ())
