module App.Docs (
  userGuideUrl,
  newIssueUrl,
) where

import App.Version (versionNumber)
import Data.Text (Text)

sourceCodeUrl :: Text
sourceCodeUrl = "https://github.com/fossas/spectrometer"

userGuideUrl :: Text
userGuideUrl = guidePathOf $ maybe "master" ("v" <>) versionNumber
  where
    guidePathOf :: Text -> Text
    guidePathOf revision = sourceCodeUrl <> "/blob/" <> revision <> "/docs/userguide.md"

newIssueUrl :: Text
newIssueUrl = sourceCodeUrl <> "/issues/new"
