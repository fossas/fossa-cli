module App.Docs (
  userGuideUrl,
  newIssueUrl,
  fossaYmlDocUrl,
  strategyLangDocUrl,
) where

import App.Version (versionOrBranch)
import Data.Text (Text)

sourceCodeUrl :: Text
sourceCodeUrl = "https://github.com/fossas/fossa-cli"

guidePathOf :: Text -> Text -> Text
guidePathOf revision repoRelUrl = sourceCodeUrl <> "/blob/" <> revision <> repoRelUrl

userGuideUrl :: Text
userGuideUrl = guidePathOf versionOrBranch "/docs/README.md"

fossaYmlDocUrl :: Text
fossaYmlDocUrl = guidePathOf versionOrBranch "/docs/references/files/fossa-yml.md"

newIssueUrl :: Text
newIssueUrl = sourceCodeUrl <> "/issues/new"

strategyLangDocUrl :: Text -> Text
strategyLangDocUrl path = guidePathOf versionOrBranch ("/docs/references/strategies/languages/" <> path)
