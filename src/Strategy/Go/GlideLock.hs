module Strategy.Go.GlideLock
  ( discover
  , analyze

  , GlideLockfile(..)
  , GlideDep(..)

  , buildGraph
  )
  where

import Prologue hiding ((.=))

import Control.Carrier.Error.Either
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "glide.lock") files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "golang-glidelock" GolangGroup $ analyze file

  walkContinue

analyze :: ( Has ReadFS sig m , Has (Error ReadFSErr) sig m) => Path Rel File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsYaml @GlideLockfile file

mkProjectClosure :: Path Rel File -> GlideLockfile -> ProjectClosureBody
mkProjectClosure file lock = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph lock
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = NotComplete
    }

buildGraph :: GlideLockfile -> Graphing Dependency
buildGraph lockfile = Graphing.fromList (map toDependency direct)
  where
  direct = imports lockfile
  toDependency GlideDep{..}  =
    Dependency { dependencyType = GoType
               , dependencyName = depName
               , dependencyVersion = Just (CEq $ T.pack (show depVersion))
               , dependencyLocations = []
               , dependencyEnvironments = []
               , dependencyTags = M.empty
               }

data GlideLockfile = GlideLockfile
  { hash    :: Integer
  , updated :: Text
  , imports :: [GlideDep]
  } deriving (Eq, Ord, Show, Generic)

data GlideDep = GlideDep
  { depName    :: Text
  , depVersion :: Integer
  , depRepo    :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON GlideLockfile where
  parseJSON = withObject "GlideLockfile" $ \obj ->
    GlideLockfile <$> obj .: "hash"
                  <*> obj .: "updated"
                  <*> obj .: "imports"

instance FromJSON GlideDep where
  parseJSON = withObject "GlideDep" $ \obj ->
    GlideDep <$> obj .:  "name"
               <*> obj .: "version"
               <*> obj .:? "repo"
