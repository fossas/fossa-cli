module SpecHook (hook) where

import Test.Hspec

-- Provide a hook to run on the spec tree when the tests start:
-- https://hspec.github.io/parallel-spec-execution.html
hook :: Spec -> Spec
hook = parallel
