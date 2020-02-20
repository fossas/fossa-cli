module Types
  ( StrategyGroup(..)

  , ProjectClosure(..)

  , Optimal(..)
  , Complete(..)

  , ProjectDependencies(..)

  , LicenseResult(..)
  , License(..)
  , LicenseType(..)

  , HasDiscover
  , runStrategy
  , runSimpleStrategy
  ) where

import Prologue

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.TaskPool
import Control.Effect.Exception
import Control.Effect.Output
import DepTypes
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Graphing

---------- Discovery

runSimpleStrategy ::
  ( Has (Lift IO) sig m
  , Has TaskPool sig m
  , Has (Output ProjectClosure) sig m
  )
  => Text -> StrategyGroup -> TaskC m ProjectClosure -> m ()
runSimpleStrategy _ _ act = forkTask $ do
  let runIt = runError @ReadFSErr
            . runError @ExecErr
            . runReadFSIO
            . runExecIO

  mask $ \restore -> do
    (res :: Either SomeException a) <- try (restore (runIt act))
    case res of
      Left _ -> pure () -- TODO
      Right (Left _) -> pure () -- TODO
      Right (Right (Left _)) -> pure () -- TODO
      Right (Right (Right a)) -> output a

runStrategy ::
  ( Has (Lift IO) sig m
  , Has TaskPool sig m
  )
  => Text -> StrategyGroup -> TaskC m () -> m ()
runStrategy _ _ act = forkTask $ do
  let runIt = runError @ReadFSErr
            . runError @ExecErr
            . runReadFSIO
            . runExecIO

  mask $ \restore -> do
    (res :: Either SomeException a) <- try (restore (runIt act))
    case res of
      Left _ -> pure () -- TODO
      Right (Left _) -> pure () -- TODO
      Right (Right (Left _)) -> pure () -- TODO
      Right (Right (Right ())) -> pure ()

type TaskC m a = ExecIOC (ReadFSIOC (ErrorC ExecErr (ErrorC ReadFSErr m))) a

type HasDiscover sig m =
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has TaskPool sig m
  , Has (Output ProjectClosure) sig m
  , MonadIO m
  , Effect sig
  )

---------- Project Closures

data Optimal = Optimal | NotOptimal
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Optimal where
  toJSON Optimal    = toJSON True
  toJSON NotOptimal = toJSON False

data Complete = Complete | NotComplete
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Complete where
  toJSON Complete    = toJSON True
  toJSON NotComplete = toJSON False

data ProjectClosure = ProjectClosure
  { closureStrategyGroup :: StrategyGroup
  , closureStrategyName  :: Text -- ^ e.g., "python-pipenv". This is temporary: ProjectClosures will eventually combine several strategies into one
  , closureModuleDir     :: Path Rel Dir -- ^ the relative module directory (for grouping with other strategies)
  , closureDependencies  :: ProjectDependencies
  , closureLicenses      :: [LicenseResult]
  } deriving (Eq, Ord, Show, Generic)

data ProjectDependencies = ProjectDependencies
  { dependenciesGraph    :: Graphing Dependency
  , dependenciesOptimal  :: Optimal -- ^ Whether this dependency graph is considered "optimal" -- i.e., best case analysis for this given project. Notably, this __does not__ imply "complete".
  , dependenciesComplete :: Complete -- ^ Whether this dependency graph contains all dependencies for a given project. When @NotComplete@, the backend will run a hasGraph-like analysis to produce the missing dependencies and graph edges
  } deriving (Eq, Ord, Show, Generic)

data StrategyGroup =
    CarthageGroup
  | DotnetGroup
  | GolangGroup
  | GradleGroup
  | MavenGroup
  | NodejsGroup
  | PythonGroup
  | RubyGroup
  | CocoapodsGroup
  deriving (Eq, Ord, Show, Generic)

-- FIXME: we also need to annotate dep graphs with Path Rel File -- merge these somehow?
data LicenseResult = LicenseResult
  { licenseFile   :: Path Rel File
  , licensesFound :: [License]
  } deriving (Eq, Ord, Show, Generic)

data License = License
  { licenseType :: LicenseType
  , value       :: Text
  } deriving (Eq, Ord, Show, Generic)

data LicenseType =
          LicenseURL
        | LicenseFile
        | LicenseSPDX
        | UnknownType
          deriving (Eq, Ord, Show, Generic)

instance ToJSON License where
    toJSON License{..} = object
      [ "type"   .=  textType licenseType
      , "value"  .=  value
      ]
      
      where
        textType :: LicenseType -> Text
        textType = \case
          LicenseURL  -> "url"
          LicenseFile -> "file"
          LicenseSPDX -> "spdx"
          UnknownType -> "unknown"

instance ToJSON LicenseResult where
    toJSON LicenseResult{..} = object
      [ "filepath"  .=  licenseFile
      , "licenses"  .=  licensesFound
      ]
