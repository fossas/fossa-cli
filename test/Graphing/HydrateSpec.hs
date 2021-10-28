module Graphing.HydrateSpec (spec) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Graphing qualified
import Graphing.Hydrate (hydrate)
import Test.Hspec (Spec, describe, it, shouldBe)

data SimpleDep = SimpleDep
  { name :: Text
  , envs :: Set Text
  }
  deriving (Eq, Ord, Show)

hydrateSimple :: Graphing.Graphing SimpleDep -> Graphing.Graphing SimpleDep
hydrateSimple = hydrate (envs) (\envSet dep -> dep{envs = envSet})

topProd :: SimpleDep
topProd = SimpleDep "topProd" $ Set.singleton "prod"

topDev :: SimpleDep
topDev = SimpleDep "topDev" $ Set.singleton "dev"

topIsolated :: SimpleDep
topIsolated = SimpleDep "topIsolated" mempty

emptyProdChild :: SimpleDep
emptyProdChild = SimpleDep "prodChild" Set.empty

emptyDevChild :: SimpleDep
emptyDevChild = SimpleDep "devChild" Set.empty

emptyMutualChild :: SimpleDep
emptyMutualChild = SimpleDep "mutualChild" Set.empty

emptyMutualGrandChild :: SimpleDep
emptyMutualGrandChild = SimpleDep "mutualGrandChild" Set.empty

emptyIsolatedChild :: SimpleDep
emptyIsolatedChild = SimpleDep "isolatedChild" mempty

hydratedProdChild :: SimpleDep
hydratedProdChild = SimpleDep "prodChild" $ Set.singleton "prod"

hydratedDevChild :: SimpleDep
hydratedDevChild = SimpleDep "devChild" $ Set.singleton "dev"

hydratedMutualChild :: SimpleDep
hydratedMutualChild = SimpleDep "mutualChild" $ Set.fromList ["prod", "dev"]

hydratedMutualGrandChild :: SimpleDep
hydratedMutualGrandChild = SimpleDep "mutualGrandChild" $ Set.fromList ["prod", "dev"]

initialGraphing :: Graphing.Graphing SimpleDep
initialGraphing = directs <> edges
  where
    directs = Graphing.directs [topProd, topDev, topIsolated]
    edges =
      Graphing.edges
        [ (topProd, emptyProdChild)
        , (topProd, emptyMutualChild)
        , (topDev, emptyDevChild)
        , (topDev, emptyMutualChild)
        , (emptyProdChild, emptyMutualGrandChild)
        , (emptyDevChild, emptyMutualGrandChild)
        , (topIsolated, emptyIsolatedChild)
        ]

expectedGraphing :: Graphing.Graphing SimpleDep
expectedGraphing = directs <> edges
  where
    -- The structure of the graph doesn't change.
    directs = Graphing.directs $ Graphing.directList initialGraphing
    edges =
      Graphing.edges
        [ (topProd, hydratedProdChild)
        , (topProd, hydratedMutualChild)
        , (topDev, hydratedDevChild)
        , (topDev, hydratedMutualChild)
        , (hydratedProdChild, hydratedMutualGrandChild)
        , (hydratedDevChild, hydratedMutualGrandChild)
        , (topIsolated, emptyIsolatedChild)
        ]

spec :: Spec
spec =
  describe "Graphing Hydrator" $
    it "Should hydrate items in the graphing" $
      -- See devdocs/graph-hydration.md for explanation
      hydrateSimple initialGraphing `shouldBe` expectedGraphing
