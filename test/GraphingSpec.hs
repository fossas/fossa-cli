module GraphingSpec
  ( spec
  )
  where

import Test.Hspec
import Prelude
import Graphing
import GraphUtil

spec :: Spec
spec = do
  describe "unfold" $ do
    it "should unfold deeply" $ do
      let graph :: Graphing Int
          graph = unfold [10] (\x -> if x > 0 then [x-2] else []) id

      expectDirect [10] graph
      expectDeps [10, 8, 6, 4, 2, 0] graph
      expectEdges [(10,8), (8,6), (6,4), (4,2), (2,0)] graph
