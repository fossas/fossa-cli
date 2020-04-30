module App.VPSScan.Scan.RunSherlock
  ( Sherlock(..)
  , SherlockC(..)
  , SherlockError(..)
  , execSherlock
  ) where
import Prologue

import Control.Carrier.Error.Either
import Effect.Exec
import qualified Data.Text as T
import App.VPSScan.Types

sherlockCmdArgs :: String -> VPSOpts -> [String]
sherlockCmdArgs scanId VPSOpts{..} = [ "--scan-id", scanId
                                          , "--sherlock-api-secret-key", sherlockClientToken
                                          , "--sherlock-api-client-id", sherlockClientID
                                          , "--sherlock-api-host", sherlockUrl
                                          , "--organization-id", show organizationID
                                          , "--project-id",  T.unpack projectID
                                          , "--revision-id",  T.unpack revisionID
                                          ]
                                          where SherlockOpts{..} = vpsSherlock

----- sherlock effect

data SherlockError
  = SherlockCommandFailed Text
  deriving (Eq, Ord, Show, Generic)

data Sherlock m k
  = ExecSherlock (Path Abs Dir) Text VPSOpts (Either SherlockError () -> m k)
  deriving Generic1

execSherlock :: Has Sherlock sig m => Path Abs Dir -> Text -> VPSOpts -> m (Either SherlockError ())
execSherlock basedir scanId vpsOpts = send (ExecSherlock basedir scanId vpsOpts pure)

instance HFunctor Sherlock
instance Effect Sherlock

----- production sherlock interpreter

newtype SherlockC m a = SherlockC { runSherlock :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m, Effect sig) => Algebra (Sherlock :+: sig) (SherlockC m) where
  alg (R other) = SherlockC (alg (handleCoercible other))
  alg (L (ExecSherlock basedir scanId vpsOpts@VPSOpts{..} k)) = (k =<<) . SherlockC $ do
    let SherlockOpts{..} = vpsSherlock
        sherlockCommand :: Command
        sherlockCommand = Command [sherlockCmdPath] ["scan", toFilePath basedir] Never
    result <- runExecIO $ exec basedir sherlockCommand $ sherlockCmdArgs (T.unpack scanId) vpsOpts
    case result of
      Left err -> pure (Left (SherlockCommandFailed (T.pack (show err))))
      Right _ -> pure (Right ())
