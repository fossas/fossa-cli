module App.Fossa.ReleaseGroup.Create (
  createReleaseGroup,
) where

import App.Fossa.Config.ReleaseGroup (CreateConfig)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Effect.Exec (Exec)
import Effect.Logger (Logger, logDebug, pretty)
import Text.Pretty.Simple (pShow)

createReleaseGroup ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  CreateConfig ->
  m ()
createReleaseGroup config = do
  logDebug $ "THis is the config of the create command -------- " <> pretty (pShow (config))
  pure ()