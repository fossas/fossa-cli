-- http://oleg.fi/gists/posts/2019-03-21-flag.html
module Data.Flag (
  Flag,
  fromFlag,
  toFlag,
  flagOpt,
) where

import Options.Applicative (FlagFields, Mod, Parser, switch)

-- | A Flag datatype with a phantom type argument. See link above for usage
newtype Flag a = Flag {getFlag :: Bool}
  deriving (Eq, Ord, Show)

fromFlag :: a -> Flag a -> Bool
fromFlag _ = getFlag

toFlag :: a -> Bool -> Flag a
toFlag _ = Flag

-- | optparse-applicative helper
flagOpt :: a -> Mod FlagFields Bool -> Parser (Flag a)
flagOpt a fields = toFlag a <$> switch fields
