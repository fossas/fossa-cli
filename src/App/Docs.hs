module App.Docs (
  userGuideUrl,
  newIssueUrl,
  fossaDepsDocUrl,
  fossaYmlDocUrl,
  strategyLangDocUrl,
  platformDocUrl,
  fossaSslCertDocsUrl,
  fossaContainerScannerUrl,
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

fossaDepsDocUrl :: Text
fossaDepsDocUrl = guidePathOf versionOrBranch "/docs/references/files/fossa-deps.md"

newIssueUrl :: Text
newIssueUrl = sourceCodeUrl <> "/issues/new"

strategyLangDocUrl :: Text -> Text
strategyLangDocUrl path = guidePathOf versionOrBranch ("/docs/references/strategies/languages/" <> path)

platformDocUrl :: Text -> Text
platformDocUrl path = guidePathOf versionOrBranch ("/docs/references/strategies/platforms/" <> path)

fossaSslCertDocsUrl :: Text
fossaSslCertDocsUrl = guidePathOf versionOrBranch "/docs/walkthroughs/ssl-cert.md"

fossaContainerScannerUrl :: Text
fossaContainerScannerUrl = guidePathOf versionOrBranch "/docs/references/subcommands/container/scanner.md"
