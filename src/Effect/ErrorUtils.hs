module Effect.ErrorUtils 
(tagError)

where

import Control.Effect.Error
import Prologue

tagError :: Has (Error e') sig m => (e -> e') -> Either e a -> m a
tagError f (Left e) = throwError (f e)
tagError _ (Right a) = pure a