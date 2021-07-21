module Data.Set.NonEmpty (
  NonEmptySet,
  nonEmpty,
  toSet,
) where

import Data.Set qualified as S

-- | A non empty set. Inspired by non empty list.
--
-- Non empty set provides all of the benefits that NonEmpty.List
-- does but for a Set.
--
-- Use nonEmpty to create the NonEmptySet from a Set.
-- USe toSet to retrieve the Set to access any Set operations.
nonEmpty :: S.Set a -> Maybe (NonEmptySet a)
nonEmpty s
  | S.null s = Nothing
  | otherwise = Just (NonEmptySet s)

toSet :: NonEmptySet a -> S.Set a
toSet = unEmptySet

newtype NonEmptySet a = NonEmptySet {unEmptySet :: S.Set a} deriving (Eq, Ord, Show, Semigroup)
