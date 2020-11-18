{-# LANGUAGE TemplateHaskell #-}

module App.Version
  ( versionNumber,
    fullVersionDescription,
    isDirty,
  )
where

import App.Version.TH
import Data.Text (Text)
import qualified Data.Text as T
import GitHash
import System.Info
import Data.Version (showVersion)

versionNumber :: Maybe Text
versionNumber = $$(getCurrentTag)

info :: GitInfo
info = $$(tGitInfoCwd)

currentBranch :: Text
currentBranch = T.pack $ giBranch info

currentCommit :: Text
currentCommit = T.pack $ giHash info

shortCommit :: Text
shortCommit = T.take 12 currentCommit

isDirty :: Bool
isDirty = giDirty info

compilerId :: Text
compilerId = name <> "-" <> version
  where
    name = T.pack compilerName
    version = T.pack $ showVersion compilerVersion

fullVersionDescription :: Text
fullVersionDescription = T.concat items
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
      [ "spectrometer: ",
        version,
        " (revision ",
        shortCommit,
        dirty,
        " compiled with ",
        compilerId,
        ")"
      ]