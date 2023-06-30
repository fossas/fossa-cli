{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Fossa.Analyze.Debug (
  collectDebugBundle,
  collectSystemInfo,
  SystemInfo (..),

  -- * Debug individual effects
  DiagDebugC,
  diagToDebug,
  ReadFSDebugC,
  readFSToDebug,
  ExecDebugC,
  execToDebug,
  LogDebugC,
  logToDebug,

  -- * Debug all effects
  debugEverything,
) where

import App.Fossa.EmbeddedBinary (themisVersion)
import App.Version (fullVersionDescription)
import Control.Carrier.Debug (
  Algebra (..),
  Debug,
  DebugC,
  Has,
  Scope,
  debugError,
  debugLog,
  debugScope,
  ignoring,
  recording,
  runDebug,
  type (:+:) (..),
 )
import Control.Carrier.Lift (sendIO)
import Control.Carrier.Simple (SimpleC, interpret, sendSimple)
import Control.Effect.Diagnostics (Diag (Fatal), Diagnostics)
import Control.Effect.Lift (Lift)
import Control.Effect.Record (Journal, RecordC, runRecord)
import Control.Effect.Stack (Stack (Context))
import Control.Effect.Sum (Member, inj)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (bimap)
import Data.Map qualified as Map
import Data.String.Conversion (toText)
import Data.Text (Text, toLower)
import Data.Word (Word64)
import Diag.Result (Result (..))
import Effect.Exec (Exec, ExecF (..))
import Effect.Logger (Logger, LoggerF (..))
import Effect.ReadFS (ReadFS, ReadFSF (..))
import GHC.Conc qualified as Conc
import GHC.Generics (Generic)
import GHC.Stats qualified as Stats
import System.Args (getCommandArgs)
import System.Environment (getEnvironment)
import System.Info qualified as Info

collectEnvVariables :: Has (Lift IO) sig m => m (Map.Map Text Text)
collectEnvVariables =
  (Map.filterWithKey onlyEnvOfInterest <$> Map.fromList)
    . map (bimap toText toText)
    <$> sendIO getEnvironment
  where
    -- environment variables are case-sensitive in linux,
    -- but are not case sensitive in windows, so always
    -- perform case-insensitive comparison
    onlyEnvOfInterest :: Text -> Text -> Bool
    onlyEnvOfInterest envName _ =
      toLower envName `elem` envOfInterest

    envOfInterest :: [Text]
    envOfInterest =
      map
        toLower
        [ -- Generic system
          "PATH"
        , "PATHEXT"
        , -- Golang
          -- Ref: https://pkg.go.dev/cmd/go#hdr-Environment_variables
          "GCCGO"
        , "GO111MODULE"
        , "GOARCH"
        , "GOBIN"
        , "GOCACHE"
        , "GODEBUG"
        , "GOENV"
        , "GOFLAGS"
        , "GOINSECURE"
        , "GOMODCACHE"
        , "GONOPROXY "
        , "GONOSUMDB"
        , "GOOS"
        , "GOPATH"
        , "GOPRIVATE"
        , "GOPROXY"
        , "GOROOT"
        , "GOSUMDB"
        , "GOTMPDIR"
        , "GOVCS"
        , "GOWORK"
        ]

collectDebugBundle ::
  ( Has Exec sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , ToJSON a
  ) =>
  cfg ->
  DebugEverythingC (RecordC ReadFSF (RecordC ExecF (DebugC m))) (Result a) ->
  m (DebugBundle cfg, (Result a))
collectDebugBundle cfg act = do
  (scope, (execJournal, (readFSJournal, res))) <- runDebug . runRecord @ExecF . runRecord @ReadFSF . debugEverything $ act
  sysInfo <- sendIO collectSystemInfo
  args <- sendIO getCommandArgs
  envVars <- collectEnvVariables

  let output :: Aeson.Value = case res of
        Failure _ _ -> "scan was not successful, no output"
        Success _ a -> Aeson.toJSON a

  let bundle =
        DebugBundle
          { bundleSystem = sysInfo
          , bundleCLIVersion = fullVersionDescription
          , bundleThemisVersion = themisVersion
          , bundleArgs = args
          , bundleConfig = cfg
          , bundleEnvVariables = envVars
          , bundleOutput = output
          , bundleScope = scope
          , bundleJournals =
              BundleJournals
                { bundleJournalReadFS = readFSJournal
                , bundleJournalExec = execJournal
                }
          }
  pure (bundle, res)

collectSystemInfo :: IO SystemInfo
collectSystemInfo = do
  capabilities <- Conc.getNumCapabilities
  processors <- Conc.getNumProcessors
  systemMemory <- do
    rtsStatsEnabled <- Stats.getRTSStatsEnabled
    if rtsStatsEnabled
      then do
        rtsStats <- Stats.getRTSStats
        pure (SystemMemory (Stats.max_live_bytes rtsStats) (Stats.allocated_bytes rtsStats))
      else pure (SystemMemory 0 0)
  pure $
    SystemInfo
      Info.os
      Info.arch
      capabilities
      processors
      systemMemory

data DebugBundle cfg = DebugBundle
  { bundleSystem :: SystemInfo
  , bundleCLIVersion :: Text
  , bundleThemisVersion :: Text
  , bundleArgs :: [Text]
  , bundleConfig :: cfg
  , bundleEnvVariables :: Map.Map Text Text
  , bundleOutput :: Aeson.Value
  , bundleScope :: Scope
  , bundleJournals :: BundleJournals
  }
  deriving (Show, Generic)

instance ToJSON cfg => ToJSON (DebugBundle cfg) where
  toEncoding = genericToEncoding defaultOptions

data SystemInfo = SystemInfo
  { systemInfoOs :: String
  , systemInfoArch :: String
  , systemCapabilities :: Int
  , systemProcessors :: Int
  , systemMemory :: SystemMemory
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SystemInfo where
  toEncoding = genericToEncoding defaultOptions

data SystemMemory = SystemMemory
  { systemMemoryLiveBytes :: Word64
  , systemMemoryAllocatedBytes :: Word64
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SystemMemory where
  toEncoding = genericToEncoding defaultOptions

data BundleJournals = BundleJournals
  { bundleJournalReadFS :: Journal ReadFSF
  , bundleJournalExec :: Journal ExecF
  }
  deriving (Show, Generic)

instance ToJSON BundleJournals where
  toEncoding = genericToEncoding defaultOptions

-----------------------------------------------

newtype StackDebugC m a = StackDebugC {runStackDebugC :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Transcribe 'scope' as 'debugScope'
instance (Member Stack sig, Member Debug sig, Algebra sig m) => Algebra (Stack :+: sig) (StackDebugC m) where
  alg hdl sig ctx = StackDebugC $
    case sig of
      L thing@(Context nm _) -> debugScope nm $ alg (runStackDebugC . hdl) (inj thing) ctx
      L ours -> alg (runStackDebugC . hdl) (inj ours) ctx
      R other -> alg (runStackDebugC . hdl) other ctx

stackToDebug :: StackDebugC m a -> m a
stackToDebug = runStackDebugC

-----------------------------------------------

newtype DiagDebugC m a = DiagDebugC {runDiagDebugC :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Transcribe 'fatal' as 'debugError'
instance (Member Diag sig, Member Debug sig, Algebra sig m) => Algebra (Diag :+: sig) (DiagDebugC m) where
  alg hdl sig ctx = DiagDebugC $
    case sig of
      L thing@(Fatal err) -> do
        debugError err
        alg (runDiagDebugC . hdl) (inj thing) ctx
      L ours -> alg (runDiagDebugC . hdl) (inj ours) ctx
      R other -> alg (runDiagDebugC . hdl) other ctx

diagToDebug :: DiagDebugC m a -> m a
diagToDebug = runDiagDebugC

-----------------------------------------------

type ReadFSDebugC = SimpleC ReadFSF

-- | Record ReadFS constructors which are relevant for debugging.
readFSToDebug :: (Has ReadFS sig m, Has Debug sig m) => ReadFSDebugC m a -> m a
readFSToDebug = interpret $ \case
  cons@ReadContentsBS'{} -> recording cons
  cons@ReadContentsText'{} -> recording cons
  cons@DoesFileExist{} -> recording cons
  cons@DoesDirExist{} -> recording cons
  cons@ResolveFile'{} -> recording cons
  cons@ResolveDir'{} -> recording cons
  cons@ResolvePath{} -> recording cons
  -- These are redacted, so it's safe to record them, they'll redact themselves
  cons@ReadRedactedContentsBS'{} -> recording cons
  cons@ReadRedactedContentsText'{} -> recording cons
  -- Unneeded or excessive for debug bundles
  cons@ReadContentsBSLimit'{} -> ignoring cons
  cons@ReadRedactedContentsBSLimit'{} -> ignoring cons
  cons@ListDir{} -> ignoring cons
  cons@GetIdentifier{} -> ignoring cons
  cons@GetCurrentDir{} -> ignoring cons

-----------------------------------------------

type ExecDebugC = SimpleC ExecF

execToDebug :: (Has Exec sig m, Has Debug sig m) => ExecDebugC m a -> m a
execToDebug = interpret $ \case
  cons@Exec{} -> recording cons

-----------------------------------------------

type LogDebugC = SimpleC LoggerF

logToDebug :: (Has Logger sig m, Has Debug sig m) => LogDebugC m a -> m a
logToDebug = interpret $ \case
  cons@(Log _ txt) -> do
    debugLog (toText (show txt))
    sendSimple cons
  cons@LogStdout{} -> ignoring cons

-----------------------------------------------

-- | Combine all of our debug wrappers into a mega-wrapper
type DebugEverythingC m = DiagDebugC (StackDebugC (ReadFSDebugC (ExecDebugC (LogDebugC m))))

debugEverything :: (Has Debug sig m, Has Exec sig m, Has ReadFS sig m, Has Logger sig m) => DebugEverythingC m a -> m a
debugEverything = logToDebug . execToDebug . readFSToDebug . stackToDebug . diagToDebug
