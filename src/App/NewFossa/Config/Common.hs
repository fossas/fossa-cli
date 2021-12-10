module App.NewFossa.Config.Common (
  releaseGroupMetadataOpts,
  filterOpt,
  pathOpt,
  targetOpt,
  baseDirArg,
  GlobalOpts (..),
  globalOpts,
  metadataOpts,
) where

import App.OptionExtensions (uriOption)
import App.Types (
  ProjectMetadata (ProjectMetadata),
  ReleaseGroupMetadata (ReleaseGroupMetadata),
 )
import Data.Bifunctor (Bifunctor (first))
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Discovery.Filters (targetFilterParser)
import Options.Applicative (
  Parser,
  argument,
  eitherReader,
  help,
  long,
  metavar,
  option,
  optional,
  short,
  str,
  strOption,
  switch,
  value,
 )
import Path (Dir, Path, Rel, parseRelDir)
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.URI (URI)
import Types (TargetFilter)

metadataOpts :: Parser ProjectMetadata
metadataOpts =
  ProjectMetadata
    <$> optional (strOption (long "title" <> short 't' <> help "the title of the FOSSA project. (default: the project name)"))
    <*> optional (strOption (long "project-url" <> short 'P' <> help "this repository's home page"))
    <*> optional (strOption (long "jira-project-key" <> short 'j' <> help "this repository's JIRA project key"))
    <*> optional (strOption (long "link" <> short 'L' <> help "a link to attach to the current build"))
    <*> optional (strOption (long "team" <> short 'T' <> help "this repository's team inside your organization"))
    <*> optional (strOption (long "policy" <> help "the policy to assign to this project in FOSSA"))
    <*> optional releaseGroupMetadataOpts

releaseGroupMetadataOpts :: Parser ReleaseGroupMetadata
releaseGroupMetadataOpts =
  ReleaseGroupMetadata
    <$> strOption (long "release-group-name" <> help "the name of the release group to add this project to")
    <*> strOption (long "release-group-release" <> help "the release of the release group to add this project to")

filterOpt :: Parser TargetFilter
filterOpt = option (eitherReader parseFilter) (long "filter" <> help "(deprecated) Analysis-Target filters (default: none)" <> metavar "ANALYSIS-TARGET")
  where
    parseFilter :: String -> Either String TargetFilter
    parseFilter = first errorBundlePretty . runParser targetFilterParser "(Command-line arguments)" . toText

pathOpt :: String -> Either String (Path Rel Dir)
pathOpt = first show . parseRelDir

targetOpt :: String -> Either String TargetFilter
targetOpt = first errorBundlePretty . runParser targetFilterParser "(Command-line arguments)" . toText

baseDirArg :: Parser String
baseDirArg = argument str (metavar "DIR" <> help "Set the base directory for scanning (default: current directory)" <> value ".")

data GlobalOpts = GlobalOpts
  { optDebug :: Bool
  , optBaseUrl :: Maybe URI
  , optProjectName :: Maybe Text
  , optProjectRevision :: Maybe Text
  , optAPIKey :: Maybe Text
  , optConfig :: Maybe FilePath
  }

globalOpts :: Parser GlobalOpts
globalOpts =
  GlobalOpts
    <$> switch (long "debug" <> help "Enable debug logging, and write detailed debug information to `fossa.debug.json`")
    <*> optional (uriOption (long "endpoint" <> short 'e' <> metavar "URL" <> help "The FOSSA API server base URL (default: https://app.fossa.com)"))
    <*> optional (strOption (long "project" <> short 'p' <> help "this repository's URL or VCS endpoint (default: VCS remote 'origin')"))
    <*> optional (strOption (long "revision" <> short 'r' <> help "this repository's current revision hash (default: VCS hash HEAD)"))
    <*> optional (strOption (long "fossa-api-key" <> help "the FOSSA API server authentication key (default: FOSSA_API_KEY from env)"))
    <*> optional (strOption (long "config" <> short 'c' <> help "Path to configuration file including filename (default: .fossa.yml)"))
