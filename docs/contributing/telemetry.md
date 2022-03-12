# CLI Telemetry

## Overview

The telemetry lifecycle is implemented via `bracket`. We provision `TelemetryCtx` 
during setup and during teardown and we perform sink Telemetry records. We do 
not use (producer-consumer threaded) pattern for emitting logs. This is to preserve
simplicity and limit network requests to a minimum. The current telemetry requirements 
allow us to ship a reasonably small payload (<200kb). 

Within TelemetryCtx, 

- We use `TBMQueue` for listed logs, results, and measures.
- We use `STM` for atomic counters and data containers.
- We use `TMVar` for setting a one-time sink or command information.

## Telemetry Scope and User Interface

Telemetry scope is configurable by the user. Telemetry scope can be 
configured via the following options in order of precedence: 

1. Command line option (`--with-telemetry-scope=off|full`)
2. Environment variable (`FOSSA_TELEMETRY_SCOPE=off|full`)
3. Configuration file (`telemetry-scope:off|full`)

For instance, if both the command-line option and the environment variable are provided
the telemetry scope provided via the command line will be used. 
	
Supported telemetry scopes:
- `off` - telemetry results are not captured or emitted. 
- `full` - telemetry results are uploaded to the default or specified endpoint.
	
When we do not have `ApiOpts` (e.g. API Key), we do not emit telemetry to an endpoint.
## Telemetry Sinks

When the environment variable `FOSSA_TELEMETRY_DEBUG=1` or `--debug` flag is provided, 
the telemetry sink is set to file. This will generate the file `fossa.telemetry.json` in the current working directory. 
	
Otherwise, telemetry is sunk at provided endpoint i.e. `--endpoint` or `server:` in the config file. 
If no such endpoint is provided, the base endpoint (`app.fossa.com`) is used.

## Interfaces and Examples

1. Counting feature usage via counters

```haskell
-- >> :t countUsage
-- countUsage :: Has Telemetry sig m => CountableAnalysisMetric -> m ()

experimental :: (Has Telemetry sig m) => SomeProj -> m ()
experimental (SomeProject manifestDir manifestFile) = do
  countUsage SomeProjectAnalyzedExperimentally
  pure ()
```

2. Tracking raw telemetry messages

Avoid using this interface as much as possible. It produces type-free telemetry data and 
we want to capture telemetry data that has explicit/strict data shape.

```haskell
-- >> :t trackRawLogMessage
-- trackRawLogMessage :: Has Telemetry sig m => Severity -> Doc AnsiStyle -> m ()

foo :: Has Telemetry sig m => m ()
foo = do
    trackRawLogMessage SevWarn "some messages explicitly to include in telemetry logs"
```

3. Captured system and CLI version information

This is automatically done at teardown. If we do not have version identifier, 
we consider CLI environment to be development. CLI version is set as git tag, 
or branch name. This information is exact as data collected in debug bundle.

4. Capturing errors and warnings

```haskell 
-- >> :t trackResult
-- trackResult :: Has Telemetry sig m => Result a -> m ()

bar = do
    result <- runDiagnosticsIO (diag :: DiagnosticsC m a)
    trackResult result

```

5. Capturing cpu time of a computation

```haskell
-- >> :t trackTimeSpent 
-- trackTimeSpent :: Has Telemetry sig m => Text -> m a -> m a

someComplexComputation :: Has Telemetry sig m => m ()
someComplexComputation = do
  trackTimeSpent "Npm" $ analyzeNpm 10
  trackTimeSpent "Yarn" $ analyzeYarn 10
```


### Future

We can implement `span` and `trace`s to provide capability to continuous profiling, this 
can be done by modifying `trackTimeSpent` to `trackSpan`. 

```
<---------------------------------> ~ Trace
<-------> ~ SpanA
         <----------> ~ SpanB
                     <------------> ~ SpanC
```

Ideally, we can leverage existing sdk from apm provider, or open telemetry instead of
building this capability in-house. 

