module Data.Monoid.Extra (isMempty) where

isMempty :: (Monoid a, Eq a) => a -> Bool
isMempty = (== mempty)
