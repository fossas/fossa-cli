-- |
-- Description: Functions for reading RPM information out of binary package headers
module Data.Rpm.DbHeaderBlob (
  PkgInfo (..),
  PkgConversionError (..),
  readPackageInfo,
  pkgInfoToDependency,
) where

import Container.OsRelease (OsInfo (..))
import Data.Rpm.DbHeaderBlob.Internal (
  PkgInfo (..),
  readPackageInfo,
 )
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import DepTypes (
  DepType (LinuxRPM),
  Dependency (..),
  VerConstraint (CEq),
 )

newtype PkgConversionError = PkgConversionError PkgInfo
  deriving (Show)

pkgInfoToDependency :: OsInfo -> PkgInfo -> Either PkgConversionError Dependency
pkgInfoToDependency
  (OsInfo os osVersion)
  PkgInfo
    { pkgName = Just pkgName
    , pkgVersion = Just pkgVersion
    , pkgRelease = Just pkgRelease
    , pkgEpoch = pkgEpoch
    , pkgArch = Just pkgArch
    } =
    Right $
      Dependency
        LinuxRPM
        (pkgName <> "#" <> os <> "#" <> osVersion)
        (Just version)
        mempty
        mempty
        mempty
    where
      version :: VerConstraint
      version = CEq $ pkgArch <> "#" <> epoch <> (pkgVersion <> "-" <> pkgRelease)

      epoch :: Text
      epoch = maybe "" ((<> ":") . toText . show) pkgEpoch
pkgInfoToDependency _ pkg = Left $ PkgConversionError pkg
