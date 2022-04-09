module Type.Operator (
  type ($),
) where

-- | Infix application.  Taken from the 'type-operators' package.
--
-- @
-- f :: Either String $ Maybe Int
-- =
-- f :: Either String (Maybe Int)
-- @
type f $ a = f a

infixr 2 $
