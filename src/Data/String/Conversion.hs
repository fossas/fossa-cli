{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Inspiration taken from Relude. Convenient string conversion functions
module Data.String.Conversion (
  ConvertUtf8 (..),
  ToText (..),
  ToLText (..),
  ToString (..),
  LazyStrict (..),
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import GHC.TypeLits
import Path (Path)
import Path qualified

----- ConvertUtf8

class ConvertUtf8 a b where
  encodeUtf8 :: a -> b
  decodeUtf8 :: b -> a

instance ConvertUtf8 String BS.ByteString where
  encodeUtf8 = TE.encodeUtf8 . T.pack
  decodeUtf8 = T.unpack . TE.decodeUtf8With TE.lenientDecode

instance ConvertUtf8 String BL.ByteString where
  encodeUtf8 = TLE.encodeUtf8 . TL.pack
  decodeUtf8 = TL.unpack . TLE.decodeUtf8With TE.lenientDecode

instance ConvertUtf8 T.Text BS.ByteString where
  encodeUtf8 = TE.encodeUtf8
  decodeUtf8 = TE.decodeUtf8With TE.lenientDecode

instance ConvertUtf8 T.Text BL.ByteString where
  encodeUtf8 = BL.fromStrict . TE.encodeUtf8
  decodeUtf8 = TE.decodeUtf8With TE.lenientDecode . BL.toStrict

instance ConvertUtf8 TL.Text BS.ByteString where
  encodeUtf8 = BL.toStrict . TLE.encodeUtf8
  decodeUtf8 = TLE.decodeUtf8With TE.lenientDecode . BL.fromStrict

instance ConvertUtf8 TL.Text BL.ByteString where
  encodeUtf8 = TLE.encodeUtf8
  decodeUtf8 = TLE.decodeUtf8With TE.lenientDecode

----- ToText

class ToText a where
  toText :: a -> T.Text

instance ToText String where
  toText = T.pack

instance ToText T.Text where
  toText = id

instance ToText TL.Text where
  toText = TL.toStrict

instance TypeError ( 'Text "Error: Use encodeUtf8/decodeUtf8 instead") => ToText BS.ByteString where
  toText = error "unreachable"

instance TypeError ( 'Text "Error: Use encodeUtf8/decodeUtf8 instead") => ToText BL.ByteString where
  toText = error "unreachable"

instance ToText (Path b t) where
  toText = toText . toString

----- ToLText

class ToLText a where
  toLText :: a -> TL.Text

instance ToLText String where
  toLText = TL.pack

instance ToLText T.Text where
  toLText = TL.fromStrict

instance ToLText TL.Text where
  toLText = id

instance TypeError ( 'Text "Error: Use encodeUtf8/decodeUtf8 instead") => ToLText BS.ByteString where
  toLText = error "unreachable"

instance TypeError ( 'Text "Error: Use encodeUtf8/decodeUtf8 instead") => ToLText BL.ByteString where
  toLText = error "unreachable"

instance ToLText (Path b t) where
  toLText = toLText . toString

----- ToString

class ToString a where
  toString :: a -> String

instance ToString T.Text where
  toString = T.unpack

instance ToString TL.Text where
  toString = TL.unpack

instance TypeError ( 'Text "Error: Use decodeUtf8 instead") => ToString BS.ByteString where
  toString = error "unreachable"

instance TypeError ( 'Text "Error: Use decodeUtf8 instead") => ToString BL.ByteString where
  toString = error "unreachable"

instance ToString (Path b t) where
  toString = Path.toFilePath

----- LazyStrict

-- The `| l -> s, s -> l` syntax comes from FunctionalDependencies.
--
-- FunctionalDependencies is useful for MultiParamTypeClasses to restrict the
-- instances you're allowed to create.
--
-- Similar to how database primary/unique keys describe relationships between
-- uniquely-identifying keys and rows, a functional dependency describes a
-- uniquely-identifying relationship between types and typeclass instances
--
-- In this case, we have two such "unique keys":
-- - `l -> s`, which says that the type of `l` uniquely maps to the type of `s`
-- - `s -> l`, which says that the type of `s` uniquely maps to the type of `l`
--
-- This is important to restrict the instances we can create, and also
-- drastically improves type inference.
--
-- Without the functional dependencies, we'd be allowed to create instances like:
--
--     instance LazyStrict Foo Bar where
--     instance LazyStrict Foo Baz where
--     instance LazyStrict Foo Quux where
--
-- ..which makes things really hard when we call `toStrict someFoo`: which of
-- the instances are we referring to?
--
-- But even if we were to define only one such instance:
--
--     instance LazyStrict Foo Bar where
--
-- ..other instances are still permitted, and the type checker can't be
-- convinced otherwise. You'd nearly always have to use type annotations:
--
--     toStrict someFoo :: Bar
class LazyStrict l s | l -> s, s -> l where
  toLazy :: s -> l
  toStrict :: l -> s

instance LazyStrict BL.ByteString BS.ByteString where
  toLazy = BL.fromStrict
  toStrict = BL.toStrict

instance LazyStrict TL.Text T.Text where
  toLazy = TL.fromStrict
  toStrict = TL.toStrict
