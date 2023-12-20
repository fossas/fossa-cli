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
