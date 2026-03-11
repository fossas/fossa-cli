module App.Fossa.Config.DumpBinaries (
  mkSubCommand,
  DumpBinsConfig (..),
  DumpBinsOpts,
) where

import App.Fossa.Config.Common (baseDirArg, validateDir)
import App.Fossa.Subcommand (EffStack, GetCommonOpts, GetSeverity, SubCommand (SubCommand))
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Options.Applicative (InfoMod, progDescDoc)
import Path (Abs, Dir, Path)
import Style (formatStringToDoc)

dumpInfo :: InfoMod a
dumpInfo = progDescDoc $ formatStringToDoc "Output all embedded binaries to specified path"

mkSubCommand :: (DumpBinsConfig -> EffStack ()) -> SubCommand DumpBinsOpts DumpBinsConfig
mkSubCommand = SubCommand "dump-binaries" dumpInfo cliParser noLoadConfig mergeOpts
  where
    cliParser = DumpBinsOpts <$> baseDirArg
    noLoadConfig = const $ pure Nothing

newtype DumpBinsOpts = DumpBinsOpts FilePath

instance GetSeverity DumpBinsOpts
instance GetCommonOpts DumpBinsOpts

newtype DumpBinsConfig = DumpBinsConfig (Path Abs Dir) deriving (Show, Generic)

instance ToJSON DumpBinsConfig where
  toEncoding = genericToEncoding defaultOptions

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Maybe FilePath ->
  a ->
  b ->
  DumpBinsOpts ->
  m DumpBinsConfig
mergeOpts _ _ _ (DumpBinsOpts path) = DumpBinsConfig <$> validateDir path
