module Strategy.Go.Transitive
  (
  )
  where

import Effect.Exec

goListCmd :: Command
goListCmd = Command
  { cmdNames = ["go"]
  , cmdBaseArgs = ["list", "-json"]
  , cmdAllowErr = Never
  }

