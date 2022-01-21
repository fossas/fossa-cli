# Diagnostics

Most commonly in haskell, failure is represented with `Either SomeErrorType a`

In effectful code, that type is `m (Either SomeErrorType a)`. `fused-effects`
gives us a convenient wrapper around that type, `ErrorC`, and operations to
"throw" and "catch" errors

```hs
newtype ErrorC e m a = ErrorC { runErrorC :: m (Either e a) }

throwError :: Has (Error e) sig m => e -> m a
catchError :: Has (Error e) sig m => m a -> (e -> m a) -> m a

example :: Has (Error Bool) sig m => m ()
example = throwError False `catchError` (\b -> if b then pure () else throwError b)
```

Unfortunately, `m (Either e a)` is not good enough for our purposes, as we have
some unusual constraints.

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

> **When all sub-actions in a fallback sequence fail, we want to aggregate and
> show _all_ errors**

### Warnings

In a similar vein with analysis fallbacks, we have the pipenv analyzer which:

1. Retrieves direct dependencies from a `Pipfile.lock` file
2. When available, uses the `pipenv` command to identify deep dependencies

Though analysis will "succeed" without (2), the analysis result will be strictly
worse.

This use-case surfaces another pair of unusual constraints:

> **Sub-action failures can cause warnings to be emitted**

> **Warnings are only emitted iff the total action succeeds**

### Error context

Low-level errors (http request errors, file read errors, command execution
errors) are not useful to users attempting to fix analysis issues.

While we don't have control over many of the low-level errors thrown, we need to
be able to annotate errors with information about troubleshooting,
documentation, and other pertinent context.

> **Error context can be attached to any action**

Further, contextualization information is _scoped_.

For example, consider the maven analyzer example above:

1. For a fatal, uncaught error thrown within the maven analyzer, we may want to
   add general "Here's documentation for maven analysis" context
2. When the maven command sub-action fails, we may want to add specific "Here's
   documentation and troubleshooting steps for the maven command" context

In the existing analyzer where (2) is immediately caught, we only want the
second piece of context attached to the error

In a world where a failure of (2) causes the whole maven analysis to fail, we
want to see both pieces of context from (1) and (2).

> **An uncaught failure inherits any error context it encounters as the failure
> propagates "upward"**

### Stack traces

Haskell regrettably does not automatically carry callstack information.

Instead, we can build our own callstack out of semantic scopes -- e.g., "Maven
analysis" or "Running `mvn` command"

> **Callstack elements can be added to an action**

Like a traditional callstack, these scopes are additive and.. scoped.

> **Sub-actions inherit the current callstack from parent actions**

These callstacks are attached to errors to provide supplementary context.

> **Errors contain the callstack in which they occurred**

As an added benefit, these callstack elements can be used as "progress"/status
messages for running tasks

> **The current callstack can be inspected**

---

## Operations

### Callstack

TODO: notes

```hs
context :: Has Diagnostics sig m => Text -> m a -> m a
getStack :: Has Diagnostics sig m => m [Text]
```

### Errors

#### Throwing errors

TODO: notes

```hs
fatal :: (ToDiagnostic err, Has Diagnostics sig m) => err -> m a
```

#### Adding error context to an action

TODO: notes

```hs
errCtx :: (ToDiagnostic ctx, Has Diagnostics sig m) => ctx -> m a -> m a
```

#### Recovering from an error

TODO: notes

```hs
recover :: Has Diagnostics sig m => m a -> m (Maybe a)
```

#### Get the result of the first of two actions to succeed

```hs
(<||>) :: Has Diagnostics sig m => m a -> m a -> m a
```

#### TODO: helper error operations: tagError, fromEither, ...

#### TODO: errorBoundary, rethrow

### Warnings

#### Emit a standalone warning

```hs
warn :: (ToDiagnostic warn, Has Diagnostics sig m) => warn -> m ()
```

#### Attach a warning to an action's failure

```hs
warnOnErr :: (ToDiagnostic warn, Has Diagnostics sig m) => warn -> m a -> m a
```

---

## Common idioms

### Analysis fallbacks with warnings

TODO: notes

```hs
mavenAnalyzer project = context "Maven" $
  mvnCommandAnalysis project <||> mvnStaticAnalysis project

mvnCommandAnalysis project =
  context "Dynamic analysis"
    . warnOnErr MissingDeps
    . warnOnErr MissingEdges
    $ MvnPlugin.analyze project

mvnStaticAnalysis project =
  context "Static analysis"
    $ MvnPom.analyze project
```

### Optional analysis actions with warnings

TODO: notes

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

--- Concrete warning types

data PipenvLockParseFailed = PipenvLockParseFailed (Path Abs File)

instance ToDiagnostic PipenvLockParseFailed where
  renderDiagnostic (PipenvLockParseFailed file) = "..."

data PipenvCmdFailed = PipenvCmdFailed (Path Abs Dir)

instance ToDiagnostic PipenvCmdFailed where
  renderDiagnostic (PipenvCmdFailed dir) = "..."
```

### Surfacing a standalone warning

While it's usually better to attach a warning to a thrown error with
`warnOnErr`, it's occasionally useful to surface standalone warnings

```hs
foo = do
  warn MyWarnType
  ...

data MyWarnType = MyWarnType

instance ToDiagnostic MyWarnType where
  renderDiagnostic MyWarnType = ...
```

---

# TODO

- caveats
  - dropped errors when caught without warning
- Result/algebra
- how to use
  - lifting Either
  - dealing with Exceptions
