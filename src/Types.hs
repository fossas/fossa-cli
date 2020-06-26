module Types
  ( StrategyGroup(..)

  , ProjectClosure(..)
  , ProjectClosureBody(..)
  , ProjectFailure(..)

  , Optimal(..)
  , Complete(..)

  , ProjectDependencies(..)

  , LicenseResult(..)
  , License(..)
  , LicenseType(..)

  , HasDiscover
  , runStrategy
  , runSimpleStrategy

  , module DepTypes
  ) where

import Prologue

import Control.Algebra
import Control.Carrier.TaskPool
import Control.Carrier.Diagnostics
import Control.Effect.Exception
import Control.Effect.Finally
import Control.Carrier.Output.IO
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
  , Has (Output ProjectFailure) sig m
  , MonadIO m
  )
  => Text -> StrategyGroup -> TaskC m ProjectClosureBody -> m ()
runSimpleStrategy name strategyGroup act = runStrategy name strategyGroup (lift act >>= output)

runStrategy ::
  ( Has (Lift IO) sig m
  , Has TaskPool sig m
  , Has (Output ProjectClosure) sig m
  , Has (Output ProjectFailure) sig m
  , MonadIO m
  )
  => Text -> StrategyGroup -> OutputC ProjectClosureBody (TaskC m) () -> m ()
runStrategy name strategyGroup act = forkTask $ do
  let runIt = runDiagnostics
            . runReadFSIO
            . runExecIO
            . runOutput @ProjectClosureBody

  mask $ \restore -> do
    (res :: Either SomeException a) <- try (restore (runIt act))
    case res of
      Left exc -> output (ProjectFailure strategyGroup name (FailureBundle [] (SomeDiagnostic [] exc)))
      Right (Left failure) -> output (ProjectFailure strategyGroup name failure)
      Right (Right result) ->
        let (bodies, ()) = resultValue result
         in traverse_ (output . toProjectClosure strategyGroup name) bodies -- TODO: warnings

type TaskC m = ExecIOC (ReadFSIOC (DiagnosticsC m))

type HasDiscover sig m =
  ( Has (Lift IO) sig m
  , Has Finally sig m
  , Has Logger sig m
  , Has TaskPool sig m
  , Has (Output ProjectClosure) sig m
  , Has (Output ProjectFailure) sig m
  , MonadIO m
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

data ProjectFailure = ProjectFailure
  { projectFailureGroup :: StrategyGroup
  , projectFailureName  :: Text
  , projectFailureCause :: FailureBundle
  } deriving (Generic)

toProjectClosure :: StrategyGroup -> Text -> ProjectClosureBody -> ProjectClosure
toProjectClosure strategyGroup name body = ProjectClosure
  { closureStrategyGroup = strategyGroup
  , closureStrategyName = name
  , closureModuleDir = bodyModuleDir body
  , closureDependencies = bodyDependencies body
  , closureLicenses = bodyLicenses body
  }

data ProjectClosureBody = ProjectClosureBody
  { bodyModuleDir    :: Path Abs Dir
  , bodyDependencies :: ProjectDependencies
  , bodyLicenses     :: [LicenseResult]
  } deriving (Eq, Ord, Show, Generic)

data ProjectClosure = ProjectClosure
  { closureStrategyGroup :: StrategyGroup
  , closureStrategyName  :: Text -- ^ e.g., "python-pipenv". This is temporary: ProjectClosures will eventually combine several strategies into one
  , closureModuleDir     :: Path Abs Dir -- ^ the absolute module directory (for grouping with other strategies)
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
  | ErlangGroup
  | GolangGroup
  | GooglesourceGroup
  | GradleGroup
  | MavenGroup
  | NodejsGroup
  | PythonGroup
  | RubyGroup
  | CocoapodsGroup
  | ClojureGroup
  | RustGroup
  | RPMGroup
  | ArchiveGroup
  deriving (Eq, Ord, Show, Generic)

-- FIXME: we also need to annotate dep graphs with Path Rel File -- merge these somehow?
data LicenseResult = LicenseResult
  { licenseFile   :: FilePath
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
