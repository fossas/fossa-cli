module Data.List.Extra ((!?)) where

-- | infix operator to access an array (similar to 'Data.List.!!')
-- if the index doesn't exist, return Nothing, otherwise
-- return Just (value)
(!?) :: [a] -> Int -> Maybe a
xs !? ix
  | length xs <= ix = Nothing
  | otherwise = Just (xs !! ix)
