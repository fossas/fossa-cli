module Effect.Error
  ( fromExceptionSem
  , fromExceptionSemVia
  )
  where

import qualified Control.Exception as X

import Prelude

import Polysemy
import Polysemy.Error
import Polysemy.Final

-- TODO: taken from polysemy until they update
fromExceptionSem :: forall e r a. (X.Exception e, Member (Error e) r, Member (Final IO) r) => Sem r a -> Sem r a
fromExceptionSem = fromExceptionSemVia @e id

fromExceptionSemVia
    :: ( X.Exception exc
       , Member (Error err) r
       , Member (Final IO) r
       )
    => (exc -> err)
    -> Sem r a
    -> Sem r a
fromExceptionSemVia f m = do
  r <- withStrategicToFinal $ do
    m' <- runS m
    s  <- getInitialStateS
    pure $ (fmap . fmap) Right m' `X.catch` \e -> (pure (Left e <$ s))
  case r of
    Left e -> throw $ f e
    Right a -> pure a
