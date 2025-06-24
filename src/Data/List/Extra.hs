module Data.List.Extra (
  (!?),
  head',
  singleton,
  chunk,
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

-- | Chunk the list into a list of lists.
-- If chunk size is 0 or lower, returns a single chunk containing the entire list.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk size as
  | size <= 0 = [as]
  | otherwise = take size as : chunk size (drop size as)
