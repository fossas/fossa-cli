module Data.Functor.Extra ((<$$>)) where

-- <$> is defined as infixl 4, so this stays consistent
infixl 4 <$$>

-- | Apply a function to a value nested in two functors.  e.g. (a -> b) -> IO (Maybe a) -> IO (Maybe b)
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
