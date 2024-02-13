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

### Error (Context, Support, Help, Documentation)

Low-level errors (http request errors, file read errors, command execution
errors) are not useful to users attempting to fix analysis issues.

While we don't have control over many of the low-level errors thrown in our
code, we'd like the ability to annotate these low-level errors with
user-friendly information about troubleshooting, documentation, and other
pertinent context.

#### Error context 

Used to provide a description of the operation being performed when an error occurred.

#### Error support

Used to prompt the user to report a problem to FOSSA support. Additionally, this may also provide users details on what to include in their report depending on the error that it is attached to. 

#### Error documentation

Used to provide a documentation reference useful for resolving an error.

#### Error help

Used to provide help text to an error. This is meant to be readable by users of the application; ideally help text is relatively terse and only displayed when you're pretty sure what the user can do to fix the problem. 


> **Error (Context, Support, Help, Documentation) can be attached to any action**

Further, we'd like these error types to be _scoped_.

Consider the maven analyzer example above:

1. For a fatal, uncaught error thrown within the maven analyzer, we may want to
   add general "Here's documentation for maven analysis"
2. When the maven command sub-action fails, we may want to add specific "Here's
   documentation and troubleshooting steps for the maven command"

In the existing analyzer where (2) is immediately caught, we only want the
command-specific (context, support, help, documentation) attached to the error.

In a world where a failure of (2) causes the whole maven analysis to fail, we
want to see both pieces of (context, support, help, documentation) from (1) and (2).

> **An uncaught failure inherits any error (context, support, help, documentation) it encounters as the uncaught
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

errCtx is used to provide a description of the operation being performed when an error ocurred. 

When the provided action succeeds, `errCtx` is a no-op:

```hs
errCtx _something (pure ()) === pure ()
errCtx _something (recover foo) === recover foo
```

#### Adding error support to an action

```hs
errSupport :: (ToDiagnostic supp, Has Diagnostics sig m) => supp -> m a -> m a

example = do
  val <- errSupport CmdFailureSupport runSomeCommand
  ...

data CmdFailureSupport
instance ToDiagnostic CmdFailureSupport
```

When the provided action fails, this operation attaches support details to the error.

errSupport is intended to provide users information about how to contact support for a given error, as well as 
any additional information that the support team might need from the user. 

When the provided action succeeds, `errSupport` is a no-op:

```hs
errSupport _something (pure ()) === pure ()
errSupport _something (recover foo) === recover foo
```

#### Adding error documentation to an action

```hs
errDoc :: (ToDiagnostic doc, Has Diagnostics sig m) => doc -> m a -> m a

example = do
  val <- errDoc CmdFailureDoc runSomeCommand
  ...

data CmdFailureDoc
instance ToDiagnostic CmdFailureDoc
```

When the provided action fails, this operation attaches documentation references to an error.

errDoc is intended to provide users with a documentation refernence that is useful for resolving an error. 

When the provided action succeeds, `errDoc` is a no-op:

```hs
errDoc _something (pure ()) === pure ()
errDoc _something (recover foo) === recover foo
```

#### Adding error help to an action

```hs
errHelp :: (ToDiagnostic hlp, Has Diagnostics sig m) => hlp -> m a -> m a

example = do
  val <- errHelp CmdFailureHelp runSomeCommand
  ...

data CmdFailureHelp
instance ToDiagnostic CmdFailureHelp
```

When the provided action fails, this operation attaches help text to an error.

errHelp is intended to provide users with actionable steps to fix the problem. Ideally, the help text is relatively terse and only used when you are pretty sure what the user can do to fix the problem. 

When the provided action succeeds, `errHelp` is a no-op:

```hs
errHelp _something (pure ()) === pure ()
errHelp _something (recover foo) === recover foo
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

### Optional analysis actions with warnings and error (context, documentation, support, help)

```hs
pipenvAnalyzer project = context "Pipenv" $ do
  direct <-
    context "Parsing Pipfile.lock for direct deps"
      . errCtx (PipenvLockParseFailed (projPipfile project))
      . errHelp PipenvLockParseFailedHelp
      . errSupport PipenvLockParseFailedSupport
      $ PipfileLock.analyze (projPipfile project)

  deep <-
    context "Running pipenv to get deep deps"
      . recover
      . warnOnErr MissingDeps
      . warnOnErr MissingEdges
      . errCtx (PipenvCmdFailed (projDir project))
      . errDoc PipenvCmdFailedDoc
      $ runPipenvCmd ...

  pure (buildGraph direct deep)

--- Concrete error context types

data PipenvLockParseFailed = PipenvLockParseFailed (Path Abs File)

instance ToDiagnostic PipenvLockParseFailed where
  renderDiagnostic (PipenvLockParseFailed file) = "..."

data PipenvCmdFailed = PipenvCmdFailed (Path Abs Dir)

instance ToDiagnostic PipenvCmdFailed where
  renderDiagnostic (PipenvCmdFailed dir) = "..."

--- Concrete error documentation types

data PipenvCmdFailedDoc = PipenvCmdFailedDoc

instance ToDiagnostic PipenvCmdFailedDoc where 
  renderDiagnostic PipenvCmdFailedDoc = "..."

--- Concrete error support types

data PipenvLockParseFailedSupport = PipenvLockParseFailedSupport

instance ToDiagnostic PipenvLockParseFailedSupport where 
  renderDiagnostic PipenvLockParseFailedSupport = "..."

--- Concrete error help types

data PipenvLockParseFailedHelp = PipenvLockParseFailedHelp

instance ToDiagnostic PipenvLockParseFailedHelp where 
  renderDiagnostic PipenvLockParseFailedHelp = "..."
```

## Rendering toDiagnostic

Rendering toDiagnostic involves constructing an Errata object. 

### Errata

The Errata module is used for creating pretty and customized error messages. 

```hs
# https://hackage.haskell.org/package/errata-0.4.0.2/docs/Errata.html

data Errata = Errata 
  {
    errataHeader :: Maybe Header -- The message that appears above all the blocks
    errataBlocks :: [Block]   -- Blocks in the source code to display
    errataBody :: Maybe Body  -- The message that appears below all blocks 
  }

data Block = Block 
  {
    blockStyle :: Style -- The style of the block
    blockLocation :: (FilePath, Line, Column) -- The filepath, line, and column of the block
    blockHeader :: Maybe Header -- The header message for the block. This will appear below the location and above the source lines. 
    blockPointers :: [Pointer] -- The blocks pointers. Used to 'point out' parts of the source code
    blockBody :: Maybe Body -- The body message for the block. This will appear below the source liens
  }
```

### Rendering for Error (Context, Support, Help, Documentation), 

These error types are used to annotate errors with user-friendly information about troubleshooting, documentation, and other pertinent context. The contents of these error types will be combined and attached to the first error in our error stack. 

When constructing a concrete error (context, support, help, documentation) type. Store the contents in the errataHeader so that we can extract the contents of all received Error (Context, Support, Help, Documentation) downstream and display them to users in a digestable format. 

```hs
pipenvAnalyzer project = context "Pipenv" $ do
  deep <-
      context "Running pipenv to get deep deps"
        . errDoc PipenvCmdFailedDoc
        $ runPipenvCmd ...

--- Concrete error documentation types
data PipenvCmdFailedDoc = PipenvCmdFailedDoc

instance ToDiagnostic PipenvCmdFailedDoc where 
  renderDiagnostic PipenvCmdFailedDoc = do
    let header = "doc/refernce/example"
    Errata (Just header) [] Nothing
```

On failure, the errata object from PipenvCmdFailedDoc will be rendered as:
`Documentation: doc/reference/example`

### Rendering errors general use case

Errata allows you construct errors that are customizable to your preference. It allows you to display source code and add pointers with various styles to your blocks. Currently, `errataBlocks` are used only in cases where we encounter fatal errors that cause the code to exit. More specifically, the `blockLocation` field is used to provide tracebacks of where errors occurred in our source code. 


Here is an example of how you might want to construct error type 

```hs
-- * Referenced from: src/Data/Error.hs 

-- SourceLocation captures the file path, line, and col at a given call site
-- SourceLocation will be used in conjuction with our errors
data SourceLocation = SourceLocation
  { filePath :: FilePath
  , line :: Int
  , col :: Int
  }
  deriving (Eq, Ord, Show, Generic)

-- getSourceLocation returns SourceLocation with the filePath, line, col of the call site
getSourceLocation :: HasCallStack => SourceLocation
getSourceLocation = case getCallStack ?callStack of
  (_, loc) : _ -> SourceLocation (srcLocFile loc) (srcLocStartLine loc) (srcLocStartCol loc)
  _ -> SourceLocation "Unknown" 0 0

-- wrapper to create an Errata block
createEmptyBlock :: SourceLocation -> Block
createEmptyBlock SourceLocation{..} =
  Block
    fancyStyle
    (filePath, line, col)
    Nothing
    []
    Nothing


-- concrete error type
data SampleError = SampleError Text SourceLocation

instance toDiagnostic SampleError where 
  renderDiagnostic (SampleError errDetails srcLoc) = 
    Errata (Just $ "Failed to peform action") (createEmptyBlock srcLoc) (Just errDetails)

exampleFunc = do 
  x <- someAction

  if x == Nothing 
    then fatal $ SampleError "error details" $ getSourceLocation callStack
    else ...

```