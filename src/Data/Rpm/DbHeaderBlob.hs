{-|
Description: Functions for reading RPM information out of binary package headers
-}
module Data.Rpm.DbHeaderBlob (
  PkgInfo (..),
  readPackageInfo,
) where

import Data.Rpm.DbHeaderBlob.Internal (
  PkgInfo (..),
  readPackageInfo,
 )
