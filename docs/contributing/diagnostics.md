# Diagnostics

Most commonly in haskell, failure is represented with `Either SomeErrorType a`

In effectful code, that type looks like `m (Either SomeErrorType a)`.
`fused-effects` gives us a convenient wrapper around that type, `ErrorC`, and
operations to "throw" and "catch" errors

```hs
newtype ErrorC e m a = ErrorC { runErrorC :: m (Either e a) }

throwError :: Has (Error e) sig m => e -> m a
catchError :: Has (Error e) sig m => m a -> (e -> m a) -> m a

example :: Has (Error Bool) sig m => m ()
example = throwError False `catchError` (\b -> if b then pure () else throwError b)
```

Unfortunately, `m (Either e a)` is not good enough for our purposes, as we have
some unusual constraints.

---

## Constraints

### Error aggregation

Analyzers have "fallbacks" to less- and less-accurate analysis techniques.

For example, the maven analyzer:

1. Attempts to do analysis with the `mvn` command
2. When the `mvn` command fails, attempts to do static analysis on `pom.xml`
   files.

When both (1) and (2) fail, we want to show _both_ errors.

This use-case surfaces the first of our unusual constraints:

> **Actions can be grouped into a fallback sequence**

> **When several sub-actions in a fallback sequence fail, we want to combine the
> failures**

### Warnings

In a similar vein with analysis fallbacks, we have the pipenv analyzer which:

1. Retrieves direct dependencies from a `Pipfile.lock` file
2. When available, uses the `pipenv` command to identify deep dependencies

Though analysis will "succeed" without (2), the analysis result will be strictly
worse.

This use-case surfaces another pair of unusual constraints:

> **Sub-action failures can cause warnings to be emitted**

> **Warnings emitted this way are only shown to the user iff the total action
> succeeds**

### Error context

Low-level errors (http request errors, file read errors, command execution
errors) are not useful to users attempting to fix analysis issues.

While we don't have control over many of the low-level errors thrown in our
code, we'd like the ability to annotate these low-level errors with
user-friendly information about troubleshooting, documentation, and other
pertinent context.

> **Error context can be attached to any action**

Further, we'd like error contextualization to be _scoped_.

Consider the maven analyzer example above:

1. For a fatal, uncaught error thrown within the maven analyzer, we may want to
   add general "Here's documentation for maven analysis" context
2. When the maven command sub-action fails, we may want to add specific "Here's
   documentation and troubleshooting steps for the maven command" context

In the existing analyzer where (2) is immediately caught, we only want the
command-specific context attached to the error.

In a world where a failure of (2) causes the whole maven analysis to fail, we
want to see both pieces of context from (1) and (2).

> **An uncaught failure inherits any error context it encounters as the uncaught
> failure propagates "upward"**

### Stack traces

Haskell regrettably does not automatically carry callstack information.

Instead, we can build our own callstack out of semantic scopes -- e.g., "Maven
analysis" or "Running `mvn` command"

> **Callstack elements can be added to an action**

Like a traditional callstack, these scopes are additive and.. scoped.

> **Sub-actions inherit the current callstack from parent actions**

These callstacks are attached to errors to provide supplementary context.

> **Errors contain the callstack in which they occurred**

---

## Syntax

### Callstack

```hs
context :: Has Diagnostics sig m => Text -> m a -> m a

example = context "foo" $ do
  res <- context "bar" $ someAction
  ...
```

Push an element onto the callstack for the duration of the provided action.

### Errors

#### Throwing errors

```hs
fatal :: (ToDiagnostic err, Has Diagnostics sig m) => err -> m a

example = do
  ...
  fatal MyErrType

data MyErrType
instance ToDiagnostic MyErrType
```

#### Adding error context to an action

```hs
errCtx :: (ToDiagnostic ctx, Has Diagnostics sig m) => ctx -> m a -> m a

example = do
  val <- errCtx CmdFailureTroubleshooting runSomeCommand
  ...

data CmdFailureTroubleshooting
instance ToDiagnostic CmdFailureTroubleshooting
```

When the provided action fails, this operation attaches context to the error.

This context will be displayed prominently above the error, and is intended for
the purpose of providing a user-friendly description, links to documentation,
troubleshooting information, etc.

When the provided action succeeds, `errCtx` is a no-op:

```hs
errCtx _something (pure ()) === pure ()
errCtx _something (recover foo) === recover foo
```

#### Recovering from an error

```hs
recover :: Has Diagnostics sig m => m a -> m (Maybe a)

example = do
  res <- recover somethingThatMightFail
  case res of
    Nothing -> ...
    Just val -> ...
```

When the provided action succeeds, return its value wrapped in Just.

When the provided action fails, emit a warning, and return Nothing.

#### Try several actions

```hs
(<||>) :: Has Diagnostics sig m => m a -> m a -> m a

example =
  tryFirstCmd <||> trySecondCmd <||> tryThirdCmd
```

Try the two provided actions, returning the value of the first successful
action.

### Warnings

#### Emit a standalone warning

```hs
warn :: (ToDiagnostic warn, Has Diagnostics sig m) => warn -> m ()
```

```hs
example = do
  warn MyWarnType

data MyWarnType
instance ToDiagnostic MyWarnType
```

#### Attach a warning to an action's failure

```hs
warnOnErr :: (ToDiagnostic warn, Has Diagnostics sig m) => warn -> m a -> m a

example = do
  val <- recover $ warnOnErr MissingEdges runSomeOptionalCommand
  ...

example2 = do
  val <- warnOnErr MissingEdges runSomeCommandToGetGraph
           <||> warnOnErr MissingDeps lessOptimalCommandToGetGraph
           <||> staticAnalysisMethod
  ...

data MissingEdges
instance ToDiagnostic MissingEdges
data MissingDeps
instance ToDiagnostic MissingDeps
```

When the provided action fails, this operation attaches a warning to the error.

This is intended to be used alongside `recover` and `<||>`, where failure of an
optional sub-action causes a non-ideal outcome.

For example, in the case of "analysis fallbacks", failure of a dynamic analysis
strategy could, e.g., prevent us from finding edges. This is the operation used
to surface those warnings.

When the provided action succeeds, this operation is a no-op:

```hs
warnOnErr _something (pure ()) === pure ()
warnOnErr _something (recover foo) === recover foo
```

---

## Cookbook

### Analysis fallbacks with warnings

```hs
mavenAnalyzer project = context "Maven" $
  mvnCommandAnalysis project <||> mvnStaticAnalysis project

mvnCommandAnalysis project =
  context "Dynamic analysis"
    . warnOnErr MissingDeps
    . warnOnErr MissingEdges
    $ MvnDepTree.analyze project

mvnStaticAnalysis project =
  context "Static analysis"
    $ MvnPom.analyze project
```

### Optional analysis actions with warnings and error context

```hs
pipenvAnalyzer project = context "Pipenv" $ do
  direct <-
    context "Parsing Pipfile.lock for direct deps"
      . errCtx (PipenvLockParseFailed (projPipfile project))
      $ PipfileLock.analyze (projPipfile project)

  deep <-
    context "Running pipenv to get deep deps"
      . recover
      . warnOnErr MissingDeps
      . warnOnErr MissingEdges
      . errCtx (PipenvCmdFailed (projDir project))
      $ runPipenvCmd ...

  pure (buildGraph direct deep)

--- Concrete error context types

data PipenvLockParseFailed = PipenvLockParseFailed (Path Abs File)

instance ToDiagnostic PipenvLockParseFailed where
  renderDiagnostic (PipenvLockParseFailed file) = "..."

data PipenvCmdFailed = PipenvCmdFailed (Path Abs Dir)

instance ToDiagnostic PipenvCmdFailed where
  renderDiagnostic (PipenvCmdFailed dir) = "..."
```
