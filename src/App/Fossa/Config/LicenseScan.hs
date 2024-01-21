module App.Fossa.Config.LicenseScan (
  mkSubCommand,
  LicenseScanConfig (..),
  LicenseScanCommand,
) where

import App.Fossa.Config.Common (baseDirArg, collectBaseDir)
import App.Fossa.Subcommand (EffStack, GetCommonOpts, GetSeverity, SubCommand (SubCommand))
import App.Types (BaseDir)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Effect.Logger (vsep)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Options.Applicative (
  Alternative ((<|>)),
  InfoMod,
  Parser,
  command,
  info,
  internal,
  progDescDoc,
  subparser,
 )
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Green))
import Style (coloredBoldItalicized, formatDoc, formatStringToDoc)

licenseScanInfo :: InfoMod a
licenseScanInfo = progDescDoc $ formatStringToDoc "Utilities for native license-scanning"

mkSubCommand :: (LicenseScanConfig -> EffStack ()) -> SubCommand LicenseScanCommand LicenseScanConfig
mkSubCommand = SubCommand "license-scan" licenseScanInfo cliParser noLoadConfig mergeOpts
  where
    noLoadConfig = const $ pure Nothing

data LicenseScanCommand
  = FossaDeps FilePath
  | DirectScan FilePath

instance GetSeverity LicenseScanCommand
instance GetCommonOpts LicenseScanCommand

data LicenseScanConfig
  = VendoredDepsOutput BaseDir
  | RawPathScan BaseDir
  deriving (Eq, Ord, Show, Generic)

instance ToJSON LicenseScanConfig where
  toEncoding = genericToEncoding defaultOptions

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  a ->
  b ->
  LicenseScanCommand ->
  m LicenseScanConfig
mergeOpts _ _ (DirectScan path) = RawPathScan <$> collectBaseDir path
mergeOpts _ _ (FossaDeps path) = VendoredDepsOutput <$> collectBaseDir path

cliParser :: Parser LicenseScanCommand
cliParser = public <|> private
  where
    public = subparser fossaDepsCommand
    private = subparser $ internal <> directScanCommand
    fossaDepsCommand =
      command
        "fossa-deps"
        ( info (FossaDeps <$> baseDirArg) $
            progDescDoc fossaDepsDesc
        )
    directScanCommand =
      command
        "direct"
        ( info (DirectScan <$> baseDirArg) $
            progDescDoc $
              formatStringToDoc "Run a license scan directly on the provided path"
        )
    fossaDepsDesc :: Maybe (Doc AnsiStyle)
    fossaDepsDesc =
      Just $
        formatDoc $
          vsep
            [ "Like " <> coloredBoldItalicized Green "fossa analyze --output" <> " but only for native scanning of vendored-dependencies"
            ]
