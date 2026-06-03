{-# LANGUAGE OverloadedRecordDot #-}

module Strategy.Node.Pnpm.V9 (
  -- * Key parsers
  getPkgNameVersionV9,

  -- * Key builders
  mkPkgKeyV9,

  -- * Environment resolver
  toEnvEmpty,

  -- * Config
  buildGraphConfigV9,
) where

import Data.HashMap.Strict qualified as HashMap
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (DepEnvironment)
import Strategy.Node.Pnpm.Types (
  BuildGraphConfig (..),
  LabelingMode (LabelingOn),
  PnpmLockFileSnapshots (snapshots),
  PnpmLockfileV9 (..),
  )
import Strategy.Node.Pnpm.V4_8 (parseAtKey)

--
-- Key parsers
--

-- | Parse a v9-style key: @name@version@ (slash optional).
--
-- >> getPkgNameVersionV9 "@angular/core@1.0.0(babel@1.0.0)" = Just ("@angular/core", "1.0.0(babel@1.0.0)")
getPkgNameVersionV9 :: Text -> Maybe (Text, Text)
getPkgNameVersionV9 = parseAtKey False

--
-- Key builders
--

-- | Build a registry package key for v9 format: @name@version@ (no leading slash)
mkPkgKeyV9 :: Text -> Text -> Text
mkPkgKeyV9 name version = name <> "@" <> version

--
-- Environment resolver
--

-- | For v9: start with empty environments; labels set them later.
toEnvEmpty :: Bool -> Set.Set DepEnvironment
toEnvEmpty _ = mempty

--
-- Config
--

-- | Config for lockfile version 9.
--
-- Snapshot edges come from the lockfile itself, so this is a function.
buildGraphConfigV9 :: PnpmLockfileV9 -> BuildGraphConfig
buildGraphConfigV9 lockFile =
  BuildGraphConfig
    { bgcGetPkgNameVersion = getPkgNameVersionV9
    , bgcMkPkgKey = mkPkgKeyV9
    , bgcToEnv = toEnvEmpty
    , bgcLabelingMode = LabelingOn
    , bgcSnapshotEdges = HashMap.toList lockFile.lockfileSnapshots.snapshots
    , bgcCatalogs = lockFile.lockfileCatalogs
    }
