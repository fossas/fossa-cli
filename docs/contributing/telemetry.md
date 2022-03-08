# CLI Telemetry

## Overview

Telemetry lifecycle is implemented via `bracket`. We provision `TelemetryCtx` 
during setup and during teardown we perform sink Telemetry records. We do 
not employ (producer-consumer threaded) pattern for emitting logs. This is to preserve
simplicity and limit network requests to minimum. Further, with current requirements 
of telemetry, we do not need to ship large amount of telemetry, so final payload 
is of reasonable small (<200kb). 

Within TelemetryCtx, 

- we use `TBMQueue` for listed logs, results, and measures
- we use `STM` for atomic counters and data containers
- we use `TMVar` for setting one time sink, or command information

## Telemetry Scope and User Interface

Telemetry scope is completely configurable by user. Telemetry scope can be 
configured via following in order of precedence: 

1. Command line option (`--with-telemetry-scope=off|full`)
2. Environment variable (`FOSSA_TELEMETRY_SCOPE=off|full`)
3. Configuration file (`telemetry-scope:off|full`)

For instance, if both command line option and environment variable provides 
telemetry scope, scope provided via command line will be used. 

When scope of `off` is used, telemetry result are not captured or emitted. 
When scope of `full` is used, provided endpoint from configuration is used as base url, to ship telemetry data. 

When we do not have `ApiOpts` (e.g. API Key), we do not emit telemetry to endpoint.

## Telemetry Sinks

When environment variable `FOSSA_TELEMETRY_DEBUG=1` or `--debug` flag is provided, 
telemetry sink is set to file. This will generate `fossa.telemetry.json` file in cwd. 

Otherwise, telemetry is sinked at provided endpoint i.e. `--endpoint` or `server:` in config file. 
If no such endpoint is provided, base endpoint (`app.fossa.com`) is used.

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

Avoid using this interface, as much as possible, since it produces type-free telemetry data. 
Ideally, we want to capture telemetry data that has explicit/strict data shape.

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



