module App.Fossa.Config.LicenseScan (
  mkSubCommand,
  LicenseScanConfig (..),
  LicenseScanOpts,
) where

import App.Fossa.Config.Common (baseDirArg, validateDir)
import App.Fossa.Subcommand (EffStack, GetCommonOpts, GetSeverity, SubCommand (SubCommand))
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Options.Applicative (InfoMod, progDesc)
import Path (Abs, Dir, Path)

licenseScanInfo :: InfoMod a
licenseScanInfo = progDesc "Runs license-scanner against a specified path"

mkSubCommand :: (LicenseScanConfig -> EffStack ()) -> SubCommand LicenseScanOpts LicenseScanConfig
mkSubCommand = SubCommand "license-scan" licenseScanInfo cliParser noLoadConfig mergeOpts
  where
    cliParser = LicenseScanOpts <$> baseDirArg
    noLoadConfig = const $ pure Nothing

newtype LicenseScanOpts = LicenseScanOpts FilePath

instance GetSeverity LicenseScanOpts
instance GetCommonOpts LicenseScanOpts

newtype LicenseScanConfig = LicenseScanConfig (Path Abs Dir) deriving (Show, Generic)

instance ToJSON LicenseScanConfig where
  toEncoding = genericToEncoding defaultOptions

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  a ->
  b ->
  LicenseScanOpts ->
  m LicenseScanConfig
mergeOpts _ _ (LicenseScanOpts path) = LicenseScanConfig <$> validateDir path
