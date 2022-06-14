{-# LANGUAGE TemplateHaskell #-}

module App.Version (
  versionNumber,
  fullVersionDescription,
  isDirty,
  currentBranch,
  versionOrBranch,
) where

import App.Version.TH (getCurrentTag)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version (showVersion)
import GitHash (GitInfo, giBranch, giDirty, giHash, tGitInfoCwdTry)
import System.Info (compilerName, compilerVersion)

versionNumber :: Maybe Text
versionNumber = $$(getCurrentTag)

info :: Either String GitInfo
info = $$(tGitInfoCwdTry)

currentBranch :: Text
currentBranch = toText $ either (const "unknown") giBranch info

currentCommit :: Text
currentCommit = toText $ either (const "unknown") giHash info

shortCommit :: Text
shortCommit = Text.take 12 currentCommit

isDirty :: Bool
isDirty = either (const False) giDirty info

compilerId :: Text
compilerId = name <> "-" <> version
  where
    name = toText compilerName
    version = toText $ showVersion compilerVersion

fullVersionDescription :: Text
fullVersionDescription = Text.concat items
  where
    version :: Text
    version =
      if isDirty
        then branch
        else maybe branch ("version " <>) versionNumber
    branch = "branch " <> currentBranch
    dirty = if isDirty then " (dirty)" else ""
    items :: [Text]
    items =
      [ "fossa-cli "
      , version
      , " (revision "
      , shortCommit
      , dirty
      , " compiled with "
      , compilerId
      , ")"
      ]

versionOrBranch :: Text
versionOrBranch = maybe currentBranch ("v" <>) versionNumber
