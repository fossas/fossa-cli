-- | A "Path"-like interface to Glob-style matching and manipulation.
-- Matching is done via "System.FilePattern", from the @filepattern@ package.
--
-- Globs can be safely created from 'Path's, or they can be unsafely created from
-- raw strings, using the 'unsafe*' functions.  Note that using unsafe functions
-- in this module can lead to incorrect behavior, specifically concerning matching.
module Data.Glob (
  -- * Matching
  matches,
  (?==),

  -- * Constructing
  append,
  (</>),
  prefixWith,
  toGlob,

  -- * Types

  -- Exporting the constuctor would make this interface unsafe
  Glob,
  unGlob,

  -- * Unsafe Constructors
  unsafeGlobAbs,
  unsafeGlobRel,
) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.String.Conversion (ToString (..), ToText (..))
import Path (Abs, Dir, Path, Rel)
import System.FilePath qualified as FP
import System.FilePattern qualified as Match

-- | A typed glob pattern.
newtype Glob a = Glob String deriving (Eq, Ord, Show, ToJSON, FromJSON)

unGlob :: Glob a -> String
unGlob (Glob s) = s

instance ToString (Glob a) where
  toString = unGlob

instance ToText (Glob a) where
  toText = toText . toString

-- | Safely promote a relative glob to an absolute glob.
prefixWith :: Path Abs Dir -> Glob Rel -> Glob Abs
prefixWith root rel = toGlob root </> unGlob rel

-- | Convert a valid path to a glob of the same base.
toGlob :: Path b t -> Glob b
toGlob = Glob . toString

-- | Infix of 'matches'
(?==) :: Glob b -> Path b t -> Bool
(?==) = flip matches

-- | Test whether a specific glob matches a specific path.
matches :: Path b t -> Glob b -> Bool
matches path glob = unGlob glob Match.?== toString path

-- | Infix of 'append'
(</>) :: Glob b -> String -> Glob b
(</>) = flip append

-- | Append a filepath to an existing glob.
append :: String -> Glob b -> Glob b
append str glob = Glob $ unGlob glob FP.</> str

-- | UNSAFE: Coerce any string to a relative glob.
unsafeGlobRel :: String -> Glob Rel
unsafeGlobRel = Glob

-- | UNSAFE: Coerce any string to an absolute glob.
unsafeGlobAbs :: String -> Glob Abs
unsafeGlobAbs = Glob
