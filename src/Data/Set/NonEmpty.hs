module Data.Set.NonEmpty (
  NonEmptySet,
  nonEmpty,
  toSet,
) where

import Data.Aeson (ToJSON)
import Data.Set qualified as Set

-- | A non empty set. Inspired by non empty list.
--
-- Non empty set provides all of the benefits that NonEmpty.List
-- does but for a Set.
--
-- Use nonEmpty to create the NonEmptySet from a Set.
-- USe toSet to retrieve the Set to access any Set operations.
nonEmpty :: Set.Set a -> Maybe (NonEmptySet a)
nonEmpty s
  | Set.null s = Nothing
  | otherwise = Just (NonEmptySet s)

toSet :: NonEmptySet a -> Set.Set a
toSet = unEmptySet

newtype NonEmptySet a = NonEmptySet {unEmptySet :: Set.Set a}
  deriving (Eq, Ord, Show, Semigroup, ToJSON)
