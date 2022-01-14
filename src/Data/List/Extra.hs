module Data.List.Extra (
  (!?),
  head',
  singleton,
) where

import Data.Maybe (listToMaybe)

-- | infix operator to access an array (similar to 'Data.List.!!')
-- if the index doesn't exist, return Nothing, otherwise
-- return Just (value)
(!?) :: [a] -> Int -> Maybe a
xs !? ix = case drop ix xs of
  [] -> Nothing
  a : _ -> Just a

-- | Better name for listToMaybe
head' :: [a] -> Maybe a
head' = listToMaybe

-- | Create a one-item list from the item given
singleton :: a -> [a]
singleton = (: [])
