{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Record
  ( Recordable (..),
    RecordableValue (..),
    RecordC (..),
    runRecord,
    Journal (..),
  )
where

import Control.Algebra
import Control.Carrier.AtomicState
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Monad.Trans
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LEncoding
import Path
import System.Exit
import Unsafe.Coerce

-- | A class of "recordable" effects -- i.e. an effect whose data constructors
-- and "result values" (the @a@ in @e m a@) can be serialized to JSON values
class Recordable (r :: Type -> Type) where
  -- | Serialize a data constructor to JSON
  recordKey :: r a -> Value

  -- | Serialize an effect data constructor's "return value" to JSON
  recordValue :: r a -> a -> Value

-- | A journal contains all of the effect invocations recorded by RecordC
newtype Journal eff = Journal {unJournal :: Map Value Value}
  deriving (Eq, Ord, Show)

instance FromJSON (Journal eff) where
  parseJSON = fmap (Journal . M.fromList) . parseJSON

instance ToJSON (Journal eff) where
  toJSON = toJSON . M.toList . unJournal

-- | Wrap and record an effect; generally used with @-XTypeApplications@, e.g.,
--
-- > runRecord @SomeEffect
runRecord :: forall e sig m a. Has (Lift IO) sig m => RecordC e sig m a -> m (Journal e, a)
runRecord act = do
  (mapping, a) <- runAtomicState M.empty . runRecordC $ act
  pure (Journal mapping, a)

-- | @RecordC e sig m a@ is a pseudo-carrier for an effect @e@ with the underlying signature @sig@
newtype RecordC (e :: (Type -> Type) -> Type -> Type) (sig :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) a = RecordC
  { runRecordC :: AtomicStateC (Map Value Value) m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

-- | We can handle an arbitrary effect 'e' -- @Algebra (e :+: sig) (RecordC e sig m)@
-- ..but we require a few things:
-- 1. 'e' must also appear somewhere else in the effect stack -- @Member e sig@
-- 2. 'e' is Recordable -- @Recordable (e m)@
--
-- There's a third claim we make, not reflected in the types: in the
-- instantiated effect type 'e m a', 'm' must be a phantom type variable. This
-- is reflected in our use of 'unsafeCoerce', and is required for us to 'send'
-- the effect further down the handler stack
instance (Member e sig, Has (Lift IO) sig m, Recordable (e m)) => Algebra (e :+: sig) (RecordC e sig m) where
  alg hdl sig' ctx = RecordC $ do
    case sig' of
      L eff -> do
        let eff' = unsafeCoerce eff :: e any a
        res <- lift $ send eff'

        let values = (recordKey eff', recordValue eff' res)
        modify (uncurry M.insert values)

        pure (res <$ ctx)
      R other -> alg (runRecordC . hdl) (R other) ctx

-- | RecordableValue is essentially @ToJSON@ with a different name. We use
-- RecordableValue to avoid orphan ToJSON instances for, e.g., ByteString and
-- ExitCode
class RecordableValue a where
  toRecordedValue :: a -> Value
  default toRecordedValue :: ToJSON a => a -> Value
  toRecordedValue = toJSON

----- Stock RecordableValue instances

instance RecordableValue ()

instance RecordableValue Bool

instance RecordableValue Char

instance RecordableValue Double

instance RecordableValue Float

instance RecordableValue Int

instance RecordableValue Integer

instance RecordableValue LText.Text

instance RecordableValue Text.Text

instance RecordableValue Value

----- Composite instances

instance RecordableValue a => RecordableValue (Maybe a) where
  toRecordedValue (Just a) = toRecordedValue a
  toRecordedValue Nothing = Null

instance {-# OVERLAPPABLE #-} RecordableValue a => RecordableValue [a] where
  toRecordedValue = toJSON . map toRecordedValue

instance {-# OVERLAPPING #-} RecordableValue [Char]

instance (RecordableValue a, RecordableValue b) => RecordableValue (Either a b) where
  toRecordedValue (Left a) = object ["Left" .= toRecordedValue a]
  toRecordedValue (Right b) = object ["Right" .= toRecordedValue b]

instance (RecordableValue a, RecordableValue b) => RecordableValue (a, b) where
  toRecordedValue (a, b) = toJSON [toRecordedValue a, toRecordedValue b]

instance (RecordableValue a, RecordableValue b, RecordableValue c) => RecordableValue (a, b, c) where
  toRecordedValue (a, b, c) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d) => RecordableValue (a, b, c, d) where
  toRecordedValue (a, b, c, d) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d, RecordableValue e) => RecordableValue (a, b, c, d, e) where
  toRecordedValue (a, b, c, d, e) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d, toRecordedValue e]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d, RecordableValue e, RecordableValue f) => RecordableValue (a, b, c, d, e, f) where
  toRecordedValue (a, b, c, d, e, f) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d, toRecordedValue e, toRecordedValue f]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d, RecordableValue e, RecordableValue f, RecordableValue g) => RecordableValue (a, b, c, d, e, f, g) where
  toRecordedValue (a, b, c, d, e, f, g) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d, toRecordedValue e, toRecordedValue f, toRecordedValue g]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d, RecordableValue e, RecordableValue f, RecordableValue g, RecordableValue h) => RecordableValue (a, b, c, d, e, f, g, h) where
  toRecordedValue (a, b, c, d, e, f, g, h) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d, toRecordedValue e, toRecordedValue f, toRecordedValue g, toRecordedValue h]

----- Additional instances

instance RecordableValue BS.ByteString where
  toRecordedValue = toJSON . Encoding.decodeUtf8

instance RecordableValue BL.ByteString where
  toRecordedValue = toJSON . LEncoding.decodeUtf8

instance RecordableValue (Path a b)

instance RecordableValue ExitCode where
  toRecordedValue ExitSuccess = toJSON (0 :: Int)
  toRecordedValue (ExitFailure i) = toJSON (i :: Int)
