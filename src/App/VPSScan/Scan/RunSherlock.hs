module App.VPSScan.Scan.RunSherlock
  ( execSherlock,
  )
where

import App.VPSScan.Types
import Control.Carrier.Error.Either
import Control.Effect.Diagnostics
import qualified Data.Text as T
import Effect.Exec
import Prologue

execSherlock :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> Text -> VPSOpts -> m ()
execSherlock basedir scanId vpsOpts = void $ execThrow basedir (sherlockCommand basedir scanId vpsOpts)

sherlockCommand :: Path Abs Dir -> Text -> VPSOpts -> Command
sherlockCommand basedir scanId VPSOpts {..} =
  Command
    { cmdName = sherlockCmdPath,
      cmdArgs =
        [ "scan",
          fromAbsDir basedir,
          "--scan-id",
          T.unpack scanId,
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
        ],
      cmdAllowErr = Never
    }
  where
    SherlockOpts {..} = vpsSherlock
