{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.Conda.CondaList
  ( analyze,
    buildGraph,
    CondaEnvDeps (..),
    CondaEnvDep (..),
    -- exposed for testing
    parseCondaEnvDep,
  )
where

import Control.Carrier.Diagnostics hiding (fromMaybe)
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Effect.Exec
import Graphing (Graphing, fromList)
import Path
import Text.Megaparsec (Parsec, chunk, single, takeWhile1P)
import Types

-- conda env create --json --file <filename.yml> --dry-run --force
-- runs conda and mocks the creation of an environment based on environment.yml.
-- The command outputs json data, including the list of resolved packages.
condaEnvCmd :: Path Abs File -> Command
condaEnvCmd environmentYml =
  Command
    { cmdName = "conda",
      cmdArgs =
        [ "env",
          "create",
          "--json",
          "--file",
          toText environmentYml,
          "--dry-run",
          "--force"
        ],
      cmdAllowErr = Never
    }

buildGraph :: [CondaEnvDep] -> Graphing Dependency
buildGraph deps = Graphing.fromList (map toDependency deps)
  where
    toDependency :: CondaEnvDep -> Dependency
    toDependency CondaEnvDep {..} =
      Dependency
        { dependencyType = CondaType,
          dependencyName = name, -- needs to be enriched
          dependencyVersion = Just $ CEq version,
          dependencyLocations = [],
          dependencyEnvironments = mempty,
          dependencyTags = Map.empty
        }

analyze ::
  ( Has Exec sig m,
    Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  m (Graphing Dependency)
analyze dir file = do
  buildGraph . placeholder <$> execJson @[CondaEnvDeps] dir (condaEnvCmd file)

placeholder :: [CondaEnvDeps] -> [CondaEnvDep]
placeholder = undefined

newtype CondaEnvDeps = CondaEnvDeps
  { dependencies :: [Text]
  }
  deriving (Eq, Ord, Show)

data CondaEnvDep = CondaEnvDep
  { channel :: Text,
    platform :: Text,
    name :: Text,
    version :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON CondaEnvDeps where
  parseJSON = withObject "CondaEnvOutput" $ \obj ->
    CondaEnvDeps
      <$> obj
      .: "dependencies"

type Parser = Parsec Void Text

parseCondaEnvDep :: Parser CondaEnvDep
parseCondaEnvDep =
  CondaEnvDep
  <$> takeWhile1P (Just "channel") (/= '/')
  <* single '/'
  <*> takeWhile1P (Just "platform") (/= ':')
  <* chunk "::"
  <*> takeWhile1P (Just "package name") (/= '=')
  <* chunk "=="
  <*> takeWhile1P (Just "version") (/= '=')
