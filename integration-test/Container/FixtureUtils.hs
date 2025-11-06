module Container.FixtureUtils (
  ContainerAnalysisC,
  runContainerEffs,
) where

import App.Fossa.DebugDir (DebugDirRef, newDebugDirRef)
import Control.Carrier.Diagnostics (DiagnosticsC, runDiagnostics)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Stack (StackC, runStack)
import Control.Carrier.Telemetry (IgnoreTelemetryC, withoutTelemetry)
import Data.Function ((&))
import Diag.Result (Result)
import Effect.Exec (ExecIOC, runExecIO)
import Effect.Logger (LoggerC, Severity (SevWarn), withDefaultLogger)
import Effect.ReadFS (ReadFSIOC, runReadFSIO)
import Type.Operator (type ($))

type ContainerAnalysisC m =
  ReaderC DebugDirRef
    $ ExecIOC
    $ ReadFSIOC
    $ LoggerC
    $ DiagnosticsC
    $ StackC
    $ IgnoreTelemetryC m

runContainerEffs :: ContainerAnalysisC IO a -> IO (Result a)
runContainerEffs f = do
  debugDirRef <- newDebugDirRef
  f
    & runReader debugDirRef
    & runExecIO
    & runReadFSIO
    & withDefaultLogger SevWarn
    & runDiagnostics
    & runStack
    & withoutTelemetry
