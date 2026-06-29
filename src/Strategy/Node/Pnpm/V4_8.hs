module Strategy.Node.Pnpm.V4_8 (
  -- * Key parsers
  getPkgNameVersionV5,
  getPkgNameVersionV6,
  parseAtKey,

  -- * Key builders
  mkPkgKeyV5,
  mkPkgKeyV6,

  -- * Environment resolver
  toEnvInline,

  -- * Configs
  buildGraphConfigV4or5,
  buildGraphConfigV678,
) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (DepEnvironment (EnvDevelopment, EnvProduction))
import Strategy.Node.Pnpm.Types (BuildGraphConfig (..), LabelingMode (LabelingOff))

--
-- Key parsing utilities
--

-- | Parse an @-style key. When @slashRequired@ is 'True', the key
-- must start with @/@ (v6/v7/v8). When 'False', the slash is optional (v9).
--
-- >> parseAtKey True "/pkg-a@1.0.0" = Just ("pkg-a", "1.0.0")
-- >> parseAtKey False "pkg-a@1.0.0" = Just ("pkg-a", "1.0.0")
-- >> parseAtKey True "pkg-a@1.0.0" = Nothing
parseAtKey :: Bool -> Text -> Maybe (Text, Text)
parseAtKey slashRequired pkgKey = do
  txt <- case Text.stripPrefix "/" pkgKey of
           Nothing | slashRequired -> Nothing
           Nothing -> Just pkgKey
           Just txt  -> Just txt
  let (nameAndVersion, peerDepInfo) = Text.breakOn "(" txt
  let (nameWithSlash, version) = Text.breakOnEnd "@" nameAndVersion
  case (Text.stripSuffix "@" nameWithSlash, version) of
    (Just name, v) -> Just (name, v <> peerDepInfo)
    _ -> Nothing

--
-- Key parsers
--

-- | Parse a v5-style key: @/name/version@ (slash required).
--
-- >> getPkgNameVersionV5 "" = Nothing
-- >> getPkgNameVersionV5 "/pkg-a/1.0.0" = Just ("pkg-a", "1.0.0")
getPkgNameVersionV5 :: Text -> Maybe (Text, Text)
getPkgNameVersionV5 pkgKey = case Text.stripPrefix "/" pkgKey of
  Nothing -> Nothing
  Just txt -> do
    let (nameWithSlash, version) = Text.breakOnEnd "/" txt
    case (Text.stripSuffix "/" nameWithSlash, version) of
      (Just name, v) -> Just (name, v)
      _ -> Nothing

-- | Parse a v6/v7/v8-style key: @/name@version@ (slash required).
--
-- >> getPkgNameVersionV6 "" = Nothing
-- >> getPkgNameVersionV6 "/pkg-a@1.0.0" = Just ("pkg-a", "1.0.0")
-- >> getPkgNameVersionV6 "/@angular/core@1.0.0(babel@1.0.0)" = Just ("@angular/core", "1.0.0(babel@1.0.0)")
getPkgNameVersionV6 :: Text -> Maybe (Text, Text)
getPkgNameVersionV6 = parseAtKey True

--
-- Key builders
--

-- | Build a registry package key for v5 format: @/name/version@
mkPkgKeyV5 :: Text -> Text -> Text
mkPkgKeyV5 name version = "/" <> name <> "/" <> version

-- | Build a registry package key for v6/v7/v8 format: @/name@version@
mkPkgKeyV6 :: Text -> Text -> Text
mkPkgKeyV6 name version = "/" <> name <> "@" <> version

--
-- Environment resolver
--

-- | For v4/v5/v6/v7/v8: derive environment directly from @dev@ field.
toEnvInline :: Bool -> Set.Set DepEnvironment
toEnvInline isDev = Set.singleton (if isDev then EnvDevelopment else EnvProduction)

--
-- Per-version configs
--

-- | Config for lockfile versions 4/5.
buildGraphConfigV4or5 :: BuildGraphConfig
buildGraphConfigV4or5 =
  BuildGraphConfig
    { bgcGetPkgNameVersion = getPkgNameVersionV5
    , bgcMkPkgKey = mkPkgKeyV5
    , bgcToEnv = toEnvInline
    , bgcLabelingMode = LabelingOff
    , bgcSnapshotEdges = mempty
    , bgcCatalogs = mempty
    }

-- | Config for lockfile versions 6/7/8.
buildGraphConfigV678 :: BuildGraphConfig
buildGraphConfigV678 =
  BuildGraphConfig
    { bgcGetPkgNameVersion = getPkgNameVersionV6
    , bgcMkPkgKey = mkPkgKeyV6
    , bgcToEnv = toEnvInline
    , bgcLabelingMode = LabelingOff
    , bgcSnapshotEdges = mempty
    , bgcCatalogs = mempty
    }
