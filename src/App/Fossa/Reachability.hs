module App.Fossa.Reachability (
  reachabilitySubCommand,
)
where

import App.Fossa.Config.Common (validateExists)
import App.Fossa.EmbeddedBinary (
  BinaryPaths,
  toPath,
  withReachabilityBinary,
 )
import App.Fossa.Subcommand (EffStack, GetCommonOpts, GetSeverity, SubCommand (SubCommand))
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding)
import Data.Aeson.Types (ToJSON (toEncoding))
import Data.String.Conversion (ConvertUtf8 (decodeUtf8), toText)
import Effect.Exec (AllowErr (..), Command (..), Exec, execThrow')
import Effect.Logger (Logger, logStdout)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Options.Applicative (InfoMod, argument, help, metavar, progDesc, str, value)
import Path.Extra (SomePath)

newtype ReachabilityBinsOpts = ReachabilityBinsOpts FilePath

newtype ReachabilityBinConfig = ReachabilityBinConfig SomePath deriving (Show, Generic)

instance ToJSON ReachabilityBinConfig where
  toEncoding = genericToEncoding defaultOptions

instance GetSeverity ReachabilityBinsOpts

instance GetCommonOpts ReachabilityBinsOpts

reachabilityInfo :: InfoMod a
reachabilityInfo = progDesc "Execute embedded reachability binary against path"

reachabilitySubCommand :: SubCommand ReachabilityBinsOpts ReachabilityBinConfig
reachabilitySubCommand = mkSubCommand runReachability

mkSubCommand :: (ReachabilityBinConfig -> EffStack ()) -> SubCommand ReachabilityBinsOpts ReachabilityBinConfig
mkSubCommand = SubCommand "reachability" reachabilityInfo cliParser noLoadConfig mergeOpts
  where
    cliParser = ReachabilityBinsOpts <$> basePathArg
    noLoadConfig = const $ pure Nothing
    basePathArg = argument str (metavar "PATH" <> help "Set the base directory for scanning (default: current directory)" <> value ".")

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  a ->
  b ->
  ReachabilityBinsOpts ->
  m ReachabilityBinConfig
mergeOpts _ _ (ReachabilityBinsOpts path) = ReachabilityBinConfig <$> validateExists path

runReachability ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  ReachabilityBinConfig ->
  m ()
runReachability (ReachabilityBinConfig targetPath) = logStdout . decodeUtf8 =<< withReachabilityBinary (\bin -> execThrow' $ reachabilityCommand bin targetPath)

reachabilityCommand :: BinaryPaths -> SomePath -> Command
reachabilityCommand bin target =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = ["--target", toText target, "-d"]
    , cmdAllowErr = Never
    }
