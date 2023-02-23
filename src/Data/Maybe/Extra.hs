module Data.Maybe.Extra (
  maybeTuple,
) where

maybeTuple :: a -> Maybe b -> Maybe (a, b)
maybeTuple _ Nothing = Nothing
maybeTuple a (Just b) = Just (a, b)
