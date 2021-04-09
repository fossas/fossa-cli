-- | Fused-effects wrapped functions from Path.IO
module Control.Effect.Path
  ( withSystemTempDir,
  )
where

import Control.Effect.Lift
import Path
import qualified Path.IO as PIO
import Prelude

withSystemTempDir ::
  Has (Lift IO) sig m =>
  -- | Directory name template
  String ->
  -- | Callback that can use the directory
  (Path Abs Dir -> m a) ->
  m a
withSystemTempDir name f = liftWith @IO $ \hdl ctx ->
  PIO.withSystemTempDir name (hdl . (<$ ctx) . f)
