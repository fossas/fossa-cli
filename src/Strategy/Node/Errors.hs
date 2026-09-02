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
import Effect.Logger (renderIt)
import Errata (Errata (..))
import Prettyprinter (indent, vsep)

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
  renderDiagnostic (CyclicPackageJson) = do
    let header = "Detected cyclic references between package.json files in the workspace"
    Errata (Just header) [] Nothing

data MissingNodeLockFile
  = MissingNodeLockFileCtx
  | MissingNodeLockFileHelp
instance ToDiagnostic MissingNodeLockFile where
  renderDiagnostic MissingNodeLockFileCtx = do
    let header = "Could not perform lockfile analysis for your nodejs project"
    Errata (Just header) [] Nothing
  renderDiagnostic MissingNodeLockFileHelp = do
    let header =
          renderIt $
            vsep
              [ "Ensure valid lockfile exist and is readable prior to running fossa."
              , indent 2 "For yarn package manager, you can perform: `yarn install` to install dependencies and generate lockfile."
              , indent 2 "For node package manager, you can perform: `npm install` to install dependencies and generate lockfile."
              , indent 2 "For bun package manager, you can perform: `bun install` to install dependencies and generate lockfile."
              ]
    Errata (Just header) [] Nothing
