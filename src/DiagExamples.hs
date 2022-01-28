-- FIXME: kill this module
module DiagExamples (examples) where

import Control.Carrier.Diagnostics
import Control.Carrier.Stack (Stack, runStack)
import Effect.Logger (AnsiStyle, Doc, Logger, Severity (SevError, SevInfo, SevWarn), logInfo, withDefaultLogger)
import Data.Text (Text)

examples :: IO ()
examples = runStack . withDefaultLogger SevInfo $ do
  showExample "Failing gradle" failingGradleLike
  showExample "Successful gradle" successfulGradleLike
  showExample "Failing pipenv" failingPipenvLike
  showExample "Partially-successful pipenv" partiallySuccessfulPipenvLike
  showExample "Validation" validation

showExample :: (Has Stack sig m, Has Logger sig m) => Doc AnsiStyle -> DiagnosticsC m a -> m ()
showExample name m = do
  logInfo name
  res <- runDiagnostics m
  withResult SevError SevWarn res (const (pure ()))

---------- Gradle-like

-- Gradle analysis tries to run three variants of the gradle command, and requires one to succeed:
--
-- 1. A `gradlew` "wrapper" for linux
-- 2. A `gradlew.exe` "wrapper" for windows
-- 3. `gradle` on $PATH
--
-- `failingGradleLike` simulates all three commands failing. Note that the
-- errors for each command are all grouped into one "failure" message.
--
-- `successfulGradleLike` simulates failures of (1) and (2), with a successful
-- (3). Note that _no logger message_ is produced for `successfulGradleLike`. As
-- far as we're concerned, the analysis succeeded with an optimal outcome.

failingGradleLike :: Has Diagnostics sig m => m ()
failingGradleLike = context "Gradle" $ do
  output <-
    context "Running gradle command to get dependencies"
      . errCtx GradleFailureCtx
      $ tryGradleW
        <||> tryGradleWExe
        <||> tryGradle

  context "Building dependency graph" $ pure (buildGraph output)
  where
    tryGradleW, tryGradleWExe, tryGradle :: Has Diagnostics sig m => m ()
    tryGradleW = fatalText "blah gradlew failed"
    tryGradleWExe = fatalText "blah gradlew.exe failed"
    tryGradle = fatalText "blah gradle failed"

    buildGraph :: a -> ()
    buildGraph _ = ()

successfulGradleLike :: Has Diagnostics sig m => m ()
successfulGradleLike = context "Gradle" $ do
  output <-
    context "Running gradle command to get dependencies"
      . errCtx GradleFailureCtx
      $ tryGradleW
        <||> tryGradleWExe
        <||> tryGradle

  context "Building dependency graph" $ pure (buildGraph output)
  where
    tryGradleW, tryGradleWExe, tryGradle :: Has Diagnostics sig m => m ()
    tryGradleW = fatalText "blah gradlew failed"
    tryGradleWExe = fatalText "blah gradlew.exe failed"
    tryGradle = pure ()

    buildGraph :: a -> ()
    buildGraph _ = ()

data GradleFailureCtx = GradleFailureCtx

instance ToDiagnostic GradleFailureCtx where
  renderDiagnostic _ = "something about gradle command failing, relevant documentation, and troubleshooting information"

---------- Pipenv-like

-- Pipenv analysis uses static analysis on Pipfile.lock to get direct
-- dependencies, and attempts to run `pipenv` to fill in the rest of the
-- dependency graph
--
-- Failure of static analysis on Pipfile.lock causes the entire analysis to
-- fail; filling in the deep dependencies with `pipenv` is optional
--
-- `failingPipenvLike` simulates a failure of static analysis on Pipenv.lock
--
-- `successfulPipenvLike` simulates a successful static analysis, but failing
-- `pipenv` command. Note that unlike the successful gradle example, we emit a
-- warning here because the outcome is non-optimal: it may be missing
-- dependencies
failingPipenvLike :: Has Diagnostics sig m => m ()
failingPipenvLike = context "Pipenv" $ do
  direct <-
    context "Parsing Pipfile.lock"
      . errCtx PipfileLockParseFailureCtx
      $ parsePipfileLock

  deep <-
    context "Running pipenv command"
      . recover
      . warnOnErr MissingDepsWarn
      . errCtx PipenvCmdFailureCtx
      $ runPipenvCmd

  pure (buildGraph direct deep)
  where
    parsePipfileLock, runPipenvCmd :: Has Diagnostics sig m => m ()
    parsePipfileLock = fatalText "blah parsing pipfile lock failed"
    runPipenvCmd = fatalText "blah pipenv failed"

    buildGraph :: direct -> Maybe deep -> ()
    buildGraph _ _ = ()

partiallySuccessfulPipenvLike :: Has Diagnostics sig m => m ()
partiallySuccessfulPipenvLike = context "Pipenv" $ do
  direct <-
    context "Parsing Pipfile.lock"
      . errCtx PipfileLockParseFailureCtx
      $ parsePipfileLock

  deep <-
    context "Running pipenv command"
      . recover
      . warnOnErr MissingDepsWarn
      . errCtx PipenvCmdFailureCtx
      $ runPipenvCmd

  pure (buildGraph direct deep)
  where
    parsePipfileLock, runPipenvCmd :: Has Diagnostics sig m => m ()
    parsePipfileLock = pure ()
    runPipenvCmd = fatalText "blah pipenv failed"

    buildGraph :: direct -> Maybe deep -> ()
    buildGraph _ _ = ()

data MissingDepsWarn = MissingDepsWarn

instance ToDiagnostic MissingDepsWarn where
  renderDiagnostic _ = "Analysis result may be missing dependencies"

data PipfileLockParseFailureCtx = PipfileLockParseFailureCtx

instance ToDiagnostic PipfileLockParseFailureCtx where
  renderDiagnostic _ = "something about pipfile lock parse failing, relevant documentation, and troubleshooting/bug-reporting information"

data PipenvCmdFailureCtx = PipenvCmdFailureCtx

instance ToDiagnostic PipenvCmdFailureCtx where
  renderDiagnostic _ = "something about the pipenv command failing, relevant documentation, and troubleshooting information"

---------- Validation

-- Building a diagnostics computation with applicative syntax behaves like
-- Validation: it attempts all chained actions, and accumulates failures into
-- one error group
--
-- `validation` simulates building an "effective config" from:
--
-- 1. a config file
-- 2. environment variables
-- 3. a failing config file parse
--
-- In this example, (1) and (3) both fail, and (2) is successful. Note that we
-- attempted (3) even though (1) failed. Also note that the errors and error
-- contexts are grouped in the logger message.

validation :: Has Diagnostics sig m => m ()
validation =
  buildEffectiveConfig
    <$> readConfigFile
    <*> getEnvVars
    <*> checkBasedir
  where
    buildEffectiveConfig :: file -> vars -> something -> ()
    buildEffectiveConfig _ _ _ = ()

    readConfigFile :: Has Diagnostics sig m => m ()
    readConfigFile =
      context "Reading config file"
        . errCtx @Text "something something couldn't read config, relevant documentation"
        $ fatalText "invalid yaml blah"

    getEnvVars :: Has Diagnostics sig m => m ()
    getEnvVars =
      context "Getting env vars"
        . errCtx @Text "something something failed to read env vars, this is a bug, report plz"
        $ pure ()

    checkBasedir :: Has Diagnostics sig m => m ()
    checkBasedir =
      context "Making sure scan dir exists"
        . errCtx @Text "scan target directory doesn't exist. make sure you target a real directory or whatever"
        $ fatalText "directory doesn't exist"
