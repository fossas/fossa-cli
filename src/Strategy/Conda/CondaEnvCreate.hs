{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.Conda.CondaEnvCreate (
  analyze,
  buildGraph,
  CondaEnvCreateOut (..),
  CondaEnvDep (..),
  -- exposed for testing
  parseCondaEnvDep,
  parseEnvCreateDeps,
) where

import Control.Carrier.Diagnostics (Diagnostics, Has, warn)
import Control.Monad.Combinators (some)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execJson)
import Graphing (Graphing, fromList)
import Path (Abs, Dir, File, Path)
import Text.Megaparsec (Parsec, chunk, errorBundlePretty, parse, single, takeWhile1P, try)
import Types (
  DepType (CondaType),
  Dependency (..),
  VerConstraint (CEq),
 )

-- conda env create --json --file <filename.yml> --dry-run --force
-- runs conda and mocks the creation of an environment based on environment.yml.
-- The command outputs json data, including the list of resolved packages.
condaEnvCmd :: Path Abs File -> Command
condaEnvCmd environmentYml =
  Command
    { cmdName = "conda"
    , cmdArgs =
        [ "env"
        , "create"
        , "--json"
        , "--file"
        , toText environmentYml
        , "--dry-run"
        , "--force"
        ]
    , cmdAllowErr = Never
    }

buildGraph :: [CondaEnvDep] -> Graphing Dependency
buildGraph deps = Graphing.fromList (map toDependency deps)
  where
    toDependency :: CondaEnvDep -> Dependency
    toDependency CondaEnvDep{..} =
      Dependency
        { dependencyType = CondaType
        , dependencyName = "'" <> channel <> "':" <> platform <> ":" <> name
        , dependencyVersion = Just $ CEq version
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

analyze ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  m (Graphing Dependency)
analyze dir file = do
  buildGraph <$> (parseEnvCreateDeps =<< execJson @CondaEnvCreateOut dir (condaEnvCmd file))

parseEnvCreateDeps :: (Monad m, Has Diagnostics sig m) => CondaEnvCreateOut -> m [CondaEnvDep]
parseEnvCreateDeps CondaEnvCreateOut{dependencies = deps} = do
  let (lefts, rights) = partitionEithers . map (parse parseCondaEnvDep "") $ deps
  traverse_ (warn . errorBundlePretty) lefts
  pure rights

newtype CondaEnvCreateOut = CondaEnvCreateOut {dependencies :: [Text]}
  deriving (Eq, Ord, Show)

data CondaEnvDep = CondaEnvDep
  { channel :: Text
  , platform :: Text
  , name :: Text
  , version :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON CondaEnvCreateOut where
  parseJSON = withObject "CondaEnvOutput" $ \obj ->
    CondaEnvCreateOut
      <$> obj
        .: "dependencies"

type Parser = Parsec Void Text

-- Parses a dep specification from a string like the following:
--   "conda-forge/osx-64::bzip2==1.0.8=h0d85af4_4"
--   "pkgs/main/osx-64::libgfortran5==11.3.0=h082f757_25"
-- the form being:
-- <channel>/<platform>::<package name>==<package version>=<build number>
-- Currently the build number isn't used for anything so it isn't parsed.
parseCondaEnvDep :: Parser CondaEnvDep
parseCondaEnvDep =
  CondaEnvDep
    <$> parseChannel
    <*> takeWhile1P (Just "platform") (/= ':')
    <* chunk "::"
    <*> takeWhile1P (Just "package name") (/= '=')
    <* chunk "=="
    <*> takeWhile1P (Just "version") (/= '=')
  where
    -- Parse '<str>/'.
    -- If it finds '<str>:', that is the signal that there are no remaining channel components.
    -- Don't consume '<str>:' and fail in that case.
    -- This is important for knowing where the channel string ends and the platform one begins.
    parseChannelComponent :: Parser Text
    parseChannelComponent = try $ takeWhile1P (Just "channel") (`notElem` ['/', ':']) <* single '/'

    parseChannel :: Parser Text
    parseChannel = Text.intercalate "/" <$> some parseChannelComponent
