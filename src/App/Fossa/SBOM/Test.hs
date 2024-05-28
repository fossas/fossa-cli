{-# LANGUAGE BlockArguments #-}

module App.Fossa.SBOM.Test (
  test,
) where

import App.Fossa.Config.Test (TestConfig (..))
import App.Fossa.Test (testMain)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Effect.Logger (
  Logger,
 )

test ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  TestConfig ->
  m ()
test = testMain
