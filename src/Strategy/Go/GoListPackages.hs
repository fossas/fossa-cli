{-|
 Module : Strategy.Gomodules.GoListPackages

Description : Analyze a Go project using go list -json -deps all
-}

module Strategy.Go.GoListPackages (analyze) where
import Control.Algebra (Has)
import Effect.Exec (Exec)
import Control.Effect.Diagnostics (Diagnostics)
import Path (Path, Abs, Dir)
import qualified Graphing
import DepTypes (Dependency)
import Types (GraphBreadth)

analyze ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m (Graphing.Graphing Dependency, GraphBreadth)
analyze = error "Not implemented"

