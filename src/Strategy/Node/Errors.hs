module Strategy.Node.Errors (
  MissingNodeLockFile (..),
  CyclicPackageJson (..),
  fossaNodeDocUrl,
  npmLockFileDocUrl,
  yarnLockfileDocUrl,
  yarnV2LockfileDocUrl,
) where

import App.Docs (strategyLangDocUrl)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
import Prettyprinter (Pretty (pretty), indent, vsep)

yarnLockfileDocUrl :: Text
yarnLockfileDocUrl = "https://classic.yarnpkg.com/lang/en/docs/yarn-lock/"

yarnV2LockfileDocUrl :: Text
yarnV2LockfileDocUrl = "https://yarnpkg.com/getting-started/qa#should-lockfiles-be-committed-to-the-repository"

npmLockFileDocUrl :: Text
npmLockFileDocUrl = "https://docs.npmjs.com/cli/v8/configuring-npm/package-lock-json"

fossaNodeDocUrl :: Text
fossaNodeDocUrl = strategyLangDocUrl "nodejs/nodejs.md"

data CyclicPackageJson = CyclicPackageJson
instance ToDiagnostic CyclicPackageJson where
  renderDiagnostic (CyclicPackageJson) = "We detected cyclic references between package.json files in the workspace."

data MissingNodeLockFile = MissingNodeLockFile
instance ToDiagnostic MissingNodeLockFile where
  renderDiagnostic (MissingNodeLockFile) =
    vsep
      [ "We could not perform lockfile analysis for your nodejs project."
      , ""
      , indent 2 $
          vsep
            [ "Ensure valid lockfile exist and is readable prior to running fossa."
            , indent 2 "For yarn package manager, you can perform: `yarn install` to install dependencies and generate lockfile."
            , indent 2 "For node package manager, you can perform: `npm install` to install dependencies and generate lockfile."
            ]
      , ""
      , "Refer to:"
      , indent 2 $
          vsep
            [ pretty $ "- " <> fossaNodeDocUrl
            , pretty $ "- " <> npmLockFileDocUrl
            , pretty $ "- " <> yarnLockfileDocUrl
            , pretty $ "- " <> yarnV2LockfileDocUrl
            ]
      ]
