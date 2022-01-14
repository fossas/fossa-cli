module Data.List.Extra (
  (!?),
  head',
  singleton,
) where

-- | infix operator to access an array (similar to 'Data.List.!!')
-- if the index doesn't exist, return Nothing, otherwise
-- return Just (value)
(!?) :: [a] -> Int -> Maybe a
xs !? ix = case drop ix xs of
  [] -> Nothing
  a : _ -> Just a

-- | If the list is empty, return Nothing, otherwise return the first element
head' :: [a] -> Maybe a
head' = \case
  [] -> Nothing
  (x : _) -> Just x

-- | Create a one-item list from the item given
singleton :: a -> [a]
singleton = (: [])
