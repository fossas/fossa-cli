module Strategy.Go.Types
  ( GolangPackage -- don't export GolangPackage; export the smart constructor instead
  , mkGolangPackage
  , GolangGrapher
  , GolangLabel(GolangLabelLocation) -- don't export GolangLabelVersion; export the smart constructor instead
  , mkGolangVersion

  , graphingGolang
  )
  where

import qualified Prelude as Unsafe
import           Prologue hiding (parent)

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import DepTypes
import Effect.Grapher
import Graphing

-- | A golang package is uniquely identified by its import path
newtype GolangPackage = GolangPackage { goImportPath :: Text } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for @GolangPackage@. Applies 'unvendor' to the value
mkGolangPackage :: Text -> GolangPackage
mkGolangPackage = GolangPackage . unvendor

type GolangGrapher = LabeledGrapher GolangPackage GolangLabel

data GolangLabel =
    GolangLabelVersion Text
  | GolangLabelLocation Text
  deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for GolangLabelVersion. Applies 'fixVersion' to the value
mkGolangVersion :: Text -> GolangLabel
mkGolangVersion = GolangLabelVersion . fixVersion

-- | Monomorphic interpreter for @LabeledGrapher GolangPackage@ into a @Graphing Dependency@
graphingGolang :: Monad m => LabeledGrapherC GolangPackage GolangLabel m a -> m (Graphing Dependency)
graphingGolang = withLabeling golangPackageToDependency

golangPackageToDependency :: GolangPackage -> Set GolangLabel -> Dependency
golangPackageToDependency pkg = foldr applyLabel start
  where

  start :: Dependency
  start = Dependency
    { dependencyType = GoType
    , dependencyName = goImportPath pkg
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }

  applyLabel :: GolangLabel -> Dependency -> Dependency
  applyLabel (GolangLabelVersion ver) dep = dep { dependencyVersion = Just (CEq ver) }
  applyLabel (GolangLabelLocation loc) dep = dep { dependencyLocations = loc : dependencyLocations dep }

-- replace "v0.0.0-20191212000000-abcdef+incompatible" with "abcdef"
fixVersion :: Text -> Text
fixVersion = Unsafe.last . T.splitOn "-" . T.replace "+incompatible" ""

-- replace "github.com/A/B/vendor/github.com/X/Y" with "github.com/X/Y"
unvendor :: Text -> Text
unvendor = Unsafe.last . T.splitOn "/vendor/"
