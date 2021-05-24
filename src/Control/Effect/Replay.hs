{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Replay
  ( Replayable (..),
    ReplayableValue (..),
    runReplay
  )
where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Monad.Trans
import Data.Aeson
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Control.Effect.Record
import Unsafe.Coerce
import qualified Data.Map.Strict as M
import Data.Aeson.Types (Parser)
import Control.Applicative
import Data.String.Conversion (encodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Path
import System.Exit

-- | A class of "replayable" effects -- i.e. an effect whose "result values"
-- (the @a@ in @e m a@) can be deserialized from JSON values produced by
-- 'recordValue' from 'Recordable'
class Recordable r => Replayable (r :: Type -> Type) where
  -- | Deserialize an effect data constructor's "return value" from JSON
  replay :: r a -> Value -> Maybe a

newtype ReplayC (e :: (Type -> Type) -> Type -> Type) (sig :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) a = ReplayC
  { runReplayC :: ReaderC (Map Value Value) m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

-- | Wrap an effect carrier, and replay its effects given the log produced by
-- 'runRecord'. If a log entry isn't available for a given effect invocation, we
-- pass the effect call down to the wrapped carrier
runReplay :: Journal e -> ReplayC e sig m a -> m a
runReplay (Journal mapping) = runReader mapping . runReplayC

instance (Member e sig, Has (Lift IO) sig m, Replayable (e m)) => Algebra (e :+: sig) (ReplayC e sig m) where
  alg hdl sig' ctx = ReplayC $ do
    case sig' of
      L eff -> do
        mapping <- ask @(Map Value Value)
        let eff' = unsafeCoerce eff :: e m a
        let keyVal = recordKey eff'
        case M.lookup keyVal mapping >>= replay eff' of
          Nothing -> do
            -- TODO: log warnings on key miss
            res <- lift $ send eff'
            pure (res <$ ctx)
          Just result -> pure (result <$ ctx)
      R other -> alg (runReplayC . hdl) (R other) ctx

-- | ReplayableValue is essentially @FromJSON@ with a different name. We use
-- ReplayableValue to avoid orphan FromJSON instances for, e.g., ByteString and
-- ExitCode
class ReplayableValue a where
  fromRecordedValue :: Value -> Parser a
  default fromRecordedValue :: FromJSON a => Value -> Parser a
  fromRecordedValue = parseJSON

----- Stock ReplayableValue instances

instance ReplayableValue ()

instance ReplayableValue Bool

instance ReplayableValue Char

instance ReplayableValue Double

instance ReplayableValue Float

instance ReplayableValue Int

instance ReplayableValue Integer

instance ReplayableValue LText.Text

instance ReplayableValue Text.Text

instance ReplayableValue Value

----- Composite instances

instance ReplayableValue a => ReplayableValue (Maybe a) where
  fromRecordedValue Null = pure Nothing
  fromRecordedValue x = Just <$> fromRecordedValue x

instance {-# OVERLAPPABLE #-} ReplayableValue a => ReplayableValue [a] where
  fromRecordedValue val = do
    xs <- parseJSON val
    traverse fromRecordedValue xs

instance {-# OVERLAPPING #-} ReplayableValue [Char] where
  fromRecordedValue = withText "String" (pure . T.unpack)

instance (ReplayableValue a, ReplayableValue b) => ReplayableValue (Either a b) where
  fromRecordedValue = withObject "Either" $ \obj -> do
    (Left <$> (obj .: "Left" >>= fromRecordedValue)) <|> (Right <$> (obj .: "Right" >>= fromRecordedValue))

instance (ReplayableValue a, ReplayableValue b) => ReplayableValue (a,b) where
  fromRecordedValue val = do
    [a,b] <- fromRecordedValue val
    (,) <$> fromRecordedValue a <*> fromRecordedValue b

instance (ReplayableValue a, ReplayableValue b, ReplayableValue c) => ReplayableValue (a,b,c) where
  fromRecordedValue val = do
    [a,b,c] <- fromRecordedValue val
    (,,) <$> fromRecordedValue a <*> fromRecordedValue b <*> fromRecordedValue c

instance (ReplayableValue a, ReplayableValue b, ReplayableValue c, ReplayableValue d) => ReplayableValue (a,b,c,d) where
  fromRecordedValue val = do
    [a,b,c,d] <- fromRecordedValue val
    (,,,) <$> fromRecordedValue a <*> fromRecordedValue b <*> fromRecordedValue c <*> fromRecordedValue d

instance (ReplayableValue a, ReplayableValue b, ReplayableValue c, ReplayableValue d, ReplayableValue e) => ReplayableValue (a,b,c,d,e) where
  fromRecordedValue val = do
    [a,b,c,d,e] <- fromRecordedValue val
    (,,,,) <$> fromRecordedValue a <*> fromRecordedValue b <*> fromRecordedValue c <*> fromRecordedValue d <*> fromRecordedValue e

instance (ReplayableValue a, ReplayableValue b, ReplayableValue c, ReplayableValue d, ReplayableValue e, ReplayableValue f) => ReplayableValue (a,b,c,d,e,f) where
  fromRecordedValue val = do
    [a,b,c,d,e,f] <- fromRecordedValue val
    (,,,,,) <$> fromRecordedValue a <*> fromRecordedValue b <*> fromRecordedValue c <*> fromRecordedValue d <*> fromRecordedValue e <*> fromRecordedValue f

instance (ReplayableValue a, ReplayableValue b, ReplayableValue c, ReplayableValue d, ReplayableValue e, ReplayableValue f, ReplayableValue g) => ReplayableValue (a,b,c,d,e,f,g) where
  fromRecordedValue val = do
    [a,b,c,d,e,f,g] <- fromRecordedValue val
    (,,,,,,) <$> fromRecordedValue a <*> fromRecordedValue b <*> fromRecordedValue c <*> fromRecordedValue d <*> fromRecordedValue e <*> fromRecordedValue f <*> fromRecordedValue g

instance (ReplayableValue a, ReplayableValue b, ReplayableValue c, ReplayableValue d, ReplayableValue e, ReplayableValue f, ReplayableValue g, ReplayableValue h) => ReplayableValue (a,b,c,d,e,f,g,h) where
  fromRecordedValue val = do
    [a,b,c,d,e,f,g,h] <- fromRecordedValue val
    (,,,,,,,) <$> fromRecordedValue a <*> fromRecordedValue b <*> fromRecordedValue c <*> fromRecordedValue d <*> fromRecordedValue e <*> fromRecordedValue f <*> fromRecordedValue g <*> fromRecordedValue h

----- Additional instances

instance ReplayableValue BS.ByteString where
  fromRecordedValue = fmap (encodeUtf8 @T.Text) . parseJSON

instance ReplayableValue BL.ByteString where
  fromRecordedValue = fmap (encodeUtf8 @TL.Text) . parseJSON

instance ReplayableValue (Path Abs Dir)
instance ReplayableValue (Path Abs File)
instance ReplayableValue (Path Rel Dir)
instance ReplayableValue (Path Rel File)

instance ReplayableValue ExitCode where
  fromRecordedValue val = do
    i <- parseJSON val
    case i of
      0 -> pure ExitSuccess
      _ -> pure $ ExitFailure i
