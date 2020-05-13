module App.VPSScan.Scan.RunSherlock
  ( Sherlock (..),
    SherlockC (..),
    SherlockError (..),
    execSherlock,
  )
where

import App.VPSScan.Types
import Control.Carrier.Error.Either
import qualified Data.Text as T
import Effect.Exec
import Prologue

sherlockCmdArgs :: String -> VPSOpts -> [String]
sherlockCmdArgs scanId VPSOpts {..} =
  [ "--scan-id",
    scanId,
    "--sherlock-api-secret-key",
    sherlockClientToken,
    "--sherlock-api-client-id",
    sherlockClientID,
    "--sherlock-api-host",
    sherlockUrl,
    "--organization-id",
    show organizationID,
    "--project-id",
    T.unpack projectID,
    "--revision-id",
    T.unpack revisionID
  ]
  where
    SherlockOpts {..} = vpsSherlock

----- sherlock effect

data SherlockError = SherlockCommandFailed Text
  deriving (Eq, Ord, Show, Generic)

data Sherlock m k where
  ExecSherlock :: Path Abs Dir -> Text -> VPSOpts -> Sherlock m (Either SherlockError ())

execSherlock :: Has Sherlock sig m => Path Abs Dir -> Text -> VPSOpts -> m (Either SherlockError ())
execSherlock basedir scanId vpsOpts = send (ExecSherlock basedir scanId vpsOpts)

----- production sherlock interpreter

newtype SherlockC m a = SherlockC {runSherlock :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Sherlock :+: sig) (SherlockC m) where
  alg hdl sig ctx = SherlockC $ case sig of
    R other -> alg (runSherlock . hdl) other ctx
    L (ExecSherlock basedir scanId vpsOpts@VPSOpts {..}) -> do
      let SherlockOpts {..} = vpsSherlock
          sherlockCommand :: Command
          sherlockCommand = Command [sherlockCmdPath] ["scan", toFilePath basedir] Never

      result <- runExecIO $ exec basedir sherlockCommand $ sherlockCmdArgs (T.unpack scanId) vpsOpts
      let mappedResult = case result of
            Left err -> Left (SherlockCommandFailed (T.pack (show err)))
            Right _ -> Right ()
      pure (mappedResult <$ ctx)
