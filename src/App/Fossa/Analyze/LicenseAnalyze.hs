{-
This module was formerly used in 'pathfinder'.
'pathfinder' was an executable that looked for declared licenses in project manifest files.
That's different than 'fossa analyze' which can examine project source-code and invoke build tools to find license info.

This typeclass definition has been retained because a number of analyzers implement support for detecting declared licenses and this might be of use to the CLI in the future.
-}
module App.Fossa.Analyze.LicenseAnalyze (
  LicenseAnalyzeProject (..),
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Control.Monad.IO.Class (MonadIO)
import Effect.Exec (Exec)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import Types

type TaskEffs sig m =
  ( Has (Lift IO) sig m
  , MonadIO m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has Diagnostics sig m
  )

class LicenseAnalyzeProject a where
  licenseAnalyzeProject :: TaskEffs sig m => a -> m [LicenseResult]
