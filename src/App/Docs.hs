module App.Docs (
  userGuideUrl,
  newIssueUrl,
  fossaDepsDocUrl,
  fossaYmlDocUrl,
  strategyLangDocUrl,
  platformDocUrl,
  fossaSslCertDocsUrl,
  fossaContainerScannerUrl,
  pathDependencyDocsUrl,
  staticAndDynamicStrategies,
  apiKeyUrl,
  vulnReachabilityProductDocsUrl,
  rolesDocsUrl,
  fossaConfigDocsUrl,
  apiTokenDocsUrl,
  fossaAnalyzeDefaultFilterDocUrl,
  fossaContainerAnalyzeDefaultFilterDocUrl,
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

pathDependencyDocsUrl :: Text
pathDependencyDocsUrl = guidePathOf versionOrBranch "/docs/references/experimental/path-dependency.md"

staticAndDynamicStrategies :: Text
staticAndDynamicStrategies = guidePathOf versionOrBranch "/docs/references/strategies/README.md#static-and-dynamic-strategies"

apiKeyUrl :: Text
apiKeyUrl = guidePathOf versionOrBranch "/README.md#generating-an-api-key"

vulnReachabilityProductDocsUrl :: Text
vulnReachabilityProductDocsUrl = "https://docs.fossa.com/docs/reachability"

rolesDocsUrl :: Text
rolesDocsUrl = "https://docs.fossa.com/docs/role-based-access-control"

fossaConfigDocsUrl :: Text
fossaConfigDocsUrl = guidePathOf versionOrBranch "/docs/references/files/fossa-yml.md"

apiTokenDocsUrl :: Text
apiTokenDocsUrl = "https://docs.fossa.com/docs/api-reference"

fossaAnalyzeDefaultFilterDocUrl :: Text
fossaAnalyzeDefaultFilterDocUrl = guidePathOf versionOrBranch "/docs/references/subcommands/analyze.md#what-are-the-default-path-filters"

fossaContainerAnalyzeDefaultFilterDocUrl :: Text
fossaContainerAnalyzeDefaultFilterDocUrl = guidePathOf versionOrBranch "/docs/references/subcommands/container.md#ignore-default-filters"
