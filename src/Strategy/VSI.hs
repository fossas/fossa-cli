module Strategy.VSI (
  discover,
) where

import App.Fossa.EmbeddedBinary (withWigginsBinary)
import App.Fossa.VPS.Scan.RunWiggins
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic (..),
  context,
  fromEither,
 )
import Control.Effect.Lift
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text qualified as T
import Discovery.Filters
import Effect.Exec
import Fossa.API.Types
import GHC.Generics
import Graphing
import Path
import Prettyprinter (pretty, viaShow)
import Srclib.Types (Locator (..), parseLocator)
import Types

newtype VSIProject = VSIProject
  { vsiDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show)

newtype VSILocator = VSILocator
  { unVSILocator :: T.Text
  }
  deriving (Eq, Ord, Show, Generic, FromJSON)

data ValidVSILocator = ValidVSILocator
  { validType :: DepType
  , validName :: T.Text
  , validRevision :: Maybe T.Text
  }

data VSIError = UnsupportedLocatorType Locator T.Text
  deriving (Eq, Ord, Show)

instance ToDiagnostic VSIError where
  renderDiagnostic (UnsupportedLocatorType locator ty) =
    "Unsupported locator type: " <> pretty ty <> " . Locator: " <> viaShow locator

discover :: (Has Diagnostics sig m, Has (Lift IO) rsig run, MonadIO run, Has Exec rsig run, Has Diagnostics rsig run) => AllFilters -> ApiOpts -> Path Abs Dir -> m [DiscoveredProject run]
discover filters apiOpts dir = context "VSI" $ do
  -- Right now we assume that if the VSI strategy is run, the top level root is a project to scan.
  -- In the future we will likely add heuristics to detect whether the VSI strategy will yield results.
  let wigginsOpts = generateVSIStandaloneOpts dir (toPathFilters filters) apiOpts
  pure [mkProject wigginsOpts (VSIProject dir)]

mkProject :: (Has (Lift IO) sig n, MonadIO n, Has Exec sig n, Has Diagnostics sig n) => WigginsOpts -> VSIProject -> DiscoveredProject n
mkProject wigginsOpts project =
  DiscoveredProject
    { projectType = "vsi"
    , projectBuildTargets = mempty
    , projectDependencyGraph = const $ analyze wigginsOpts
    , projectPath = vsiDir project
    , projectLicenses = pure []
    }

buildGraph :: [VSILocator] -> Either VSIError (Graphing Dependency)
buildGraph rawLocators = asAGraph
  where
    validated :: Either VSIError [ValidVSILocator]
    validated = traverse validateLocator rawLocators

    transformed :: Either VSIError [Dependency]
    transformed = map transformLocator <$> validated

    asAGraph :: Either VSIError (Graphing Dependency)
    asAGraph = Graphing.fromList <$> transformed

validateLocator :: VSILocator -> Either VSIError ValidVSILocator
validateLocator vsiLocator = do
  let locator = parseLocator $ unVSILocator vsiLocator
  ty <- toDepType locator
  pure (ValidVSILocator ty (locatorProject locator) (locatorRevision locator))

transformLocator :: ValidVSILocator -> Dependency
transformLocator locator =
  Dependency (validType locator) (validName locator) (CEq <$> validRevision locator) [] [] mempty

analyze :: (Has (Lift IO) sig m, MonadIO m, Has Exec sig m, Has Diagnostics sig m) => WigginsOpts -> m (Graphing Dependency)
analyze opts = context "VSI" $ do
  vsiLocators <- context "Running VSI binary" $ withWigginsBinary (execWigginsJson opts)
  context "Building dependency graph" $ fromEither (buildGraph vsiLocators)

toDepType :: Locator -> Either VSIError DepType
toDepType locator = case locatorFetcher locator of
  "git" -> Right GitType
  "archive" -> Right GooglesourceType
  "mvn" -> Right MavenType
  "nuget" -> Right NuGetType
  other -> Left $ UnsupportedLocatorType locator other
