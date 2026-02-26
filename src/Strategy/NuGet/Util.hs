module Strategy.NuGet.Util (
  resolvedVersion,
  isUnresolvedVariable,
) where

import Data.Text (Text)
import Data.Text qualified as Text

-- | Filter out unresolved build-time variable references from version strings.
-- MSBuild uses @$(VarName)@, NuGet .nuspec uses @$token$@ syntax.
-- These cannot be resolved at analysis time and create malformed locators.
resolvedVersion :: Maybe Text -> Maybe Text
resolvedVersion Nothing = Nothing
resolvedVersion (Just v)
  | isUnresolvedVariable v = Nothing
  | otherwise = Just v

-- | Check if a version string contains unresolved build-time variable references.
-- A @$@ character is not valid in SemVer or NuGet version strings, so any
-- version containing @$@ is almost certainly an unresolved variable reference.
isUnresolvedVariable :: Text -> Bool
isUnresolvedVariable = Text.any (== '$')
