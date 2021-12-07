{-# LANGUAGE RoleAnnotations #-}

module Data.Tagged (
  Tagged,
  enforceTag,
  unTag,
  applyTag,
) where

import GHC.Generics (Generic)

-- | The Tagged newtype allows an arbitrary tag type to accompany any other type.
-- @ty@ is representational, which is the default type role for that type parameter.
--   This means that @ty -> ty'@ can be done by coerce if the runtime representation is the same.
-- @tag@ is nominal, though the default role is phantom.  With phantom type roles, GHC allows
--   coerce for any reason, since there is no type.  However the type safety of the 'Tagged' type
--   comes from not allowing coerce to do any magic, so we use nominal, which means that coerce
--   only allows exact type matches, regardless of representation.  Essentially, @coerce = id@.
--
-- In practice:
-- @
--   data SomeTag
--   data SomeOtherTag
--
--   doCoerce :: Tagged SomeTag a -> Tagged SomeTag a
--   doCoerce = coerce
--
--   dontCoerce :: Tagged SomeTag ty -> Tagged SomeOtherTag ty
--   dontCoerce = coerce  -- Compile error: SomeTag is not SomeOtherTag
--
--   -- This is a representation of the pattern that a Tagged item avoids.
--   myInt :: Int
--   myInt = unTag @SomeOtherTag $ coerce $ applyTag @SomeTag (3 :: Int)
-- @
newtype Tagged tag ty = Tagged {inner :: ty}
  deriving
    ( Eq
    , Ord
    , Show
    , Semigroup
    , Monoid
    , Generic
    , Functor
    , Foldable
    , Traversable
    )

-- We want to inherit as much as we can from the inner type, deriving will exclude things that don't fit.

type role Tagged nominal representational

-- | Enable strict type checking of tags with @-XTypeApplications@.
-- > enforceTag @MyTag (x :: Tagged MyTag ty) = id
enforceTag :: forall tag ty. Tagged tag ty -> Tagged tag ty
enforceTag = id

-- | Same as 'enforceTag', but extracts the inner @ty@.
unTag :: forall tag ty. Tagged tag ty -> ty
unTag = inner

-- | @applyTag ty@ tags a @ty@ with the given tag.  With type applications, this can be enforced more easily.
applyTag :: forall tag ty. ty -> Tagged tag ty
applyTag = Tagged
