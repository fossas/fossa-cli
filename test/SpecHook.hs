module SpecHook (hook) where

import Test.Hspec

hook :: Spec -> Spec
hook = parallel
