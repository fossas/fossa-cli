module App.Docs (
  userGuideUrl,
  newIssueUrl,
  fossaYmlDocUrl,
) where

import App.Version (currentBranch, versionNumber)
import Data.Text (Text)

sourceCodeUrl :: Text
sourceCodeUrl = "https://github.com/fossas/fossa-cli"

guidePathOf :: Text -> Text -> Text
guidePathOf revision repoRelUrl = sourceCodeUrl <> "/blob/" <> revision <> repoRelUrl

userGuideUrl :: Text
userGuideUrl = guidePathOf (maybe currentBranch ("v" <>) versionNumber) "/docs/README.md"

fossaYmlDocUrl :: Text
fossaYmlDocUrl = guidePathOf (maybe currentBranch ("v" <>) versionNumber) "/docs/references/files/fossa-yml.md"

newIssueUrl :: Text
newIssueUrl = sourceCodeUrl <> "/issues/new"
