{-# LANGUAGE TemplateHaskell #-}

module App.Version (
  versionNumber,
  fullVersionDescription,
  currentBranch,
  versionOrBranch,
  isReleaseVersion,
) where

import App.Version.TH (getCurrentTag)
import Data.Maybe (isJust)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version (showVersion)
import GitHash (GitInfo, giBranch, giHash, tGitInfoCwdTry)
import System.Info (compilerName, compilerVersion)

versionNumber :: Maybe Text
versionNumber = $$(getCurrentTag)

-- Avoid using giDirty in GitInfo.
-- The build process in CI dirties the worktree in order to ensure that the versions in this module are up to date.
info :: Either String GitInfo
info = $$(tGitInfoCwdTry)

currentBranch :: Text
currentBranch = toText $ either (const "unknown") giBranch info

currentCommit :: Text
currentCommit = toText $ either (const "unknown") giHash info

shortCommit :: Text
shortCommit = Text.take 12 currentCommit

compilerId :: Text
compilerId = name <> "-" <> version
  where
    name = toText compilerName
    version = toText $ showVersion compilerVersion

fullVersionDescription :: Text
fullVersionDescription = Text.concat items
  where
    version :: Text
    version = maybe branch ("version " <>) versionNumber
    branch = "branch " <> currentBranch
    buildType = if isReleaseVersion then "" else " (development build) "
    items :: [Text]
    items =
      [ "fossa-cli "
      , version
      , " (revision "
      , shortCommit
      , buildType
      , " compiled with "
      , compilerId
      , ")"
      ]

versionOrBranch :: Text
versionOrBranch = maybe currentBranch ("v" <>) versionNumber

isReleaseVersion :: Bool
isReleaseVersion = isJust versionNumber && currentBranch == "master"
