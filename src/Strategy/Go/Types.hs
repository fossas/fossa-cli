module Strategy.Go.Types
  ( GolangPackage -- don't export GolangPackage; export the smart constructor instead
  , mkGolangPackage
  , GolangLabel(GolangLabelLocation) -- don't export GolangLabelVersion; export the smart constructor instead
  , mkGolangVersion

  , graphingGolang
  )
  where

import Prologue hiding (parent)

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Polysemy

import           Effect.Graphing
import qualified Graph as G

-- | A golang package is uniquely identified by its import path
newtype GolangPackage = GolangPackage { goImportPath :: Text } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for @GolangPackage@. Applies 'unvendor' to the value
mkGolangPackage :: Text -> GolangPackage
mkGolangPackage = GolangPackage . unvendor

type instance PkgLabel GolangPackage = GolangLabel

data GolangLabel =
    GolangLabelVersion Text
  | GolangLabelLocation Text
  deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for GolangLabelVersion. Applies 'fixVersion' to the value
mkGolangVersion :: Text -> PkgLabel GolangPackage
mkGolangVersion = GolangLabelVersion . fixVersion

-- | Monomorphic interpreter for @Graphing GolangPackage@ into a @G.Graph@
graphingGolang :: Sem (Graphing GolangPackage ': r) a -> Sem r G.Graph
graphingGolang = graphingToGraph golangPackageToDependency

golangPackageToDependency :: GolangPackage -> Set GolangLabel -> G.Dependency
golangPackageToDependency pkg = foldr applyLabel start
  where

  start :: G.Dependency
  start = G.Dependency
    { dependencyType = G.GoType
    , dependencyName = goImportPath pkg
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyTags = M.empty
    }

  applyLabel :: GolangLabel -> G.Dependency -> G.Dependency
  applyLabel (GolangLabelVersion ver) dep = dep { G.dependencyVersion = Just (G.CEq ver) }
  applyLabel (GolangLabelLocation loc) dep = dep { G.dependencyLocations = loc : G.dependencyLocations dep }

-- replace "v0.0.0-20191212000000-abcdef+incompatible" with "abcdef"
fixVersion :: Text -> Text
fixVersion = last . T.splitOn "-" . T.replace "+incompatible" ""

-- replace "github.com/A/B/vendor/github.com/X/Y" with "github.com/X/Y"
unvendor :: Text -> Text
unvendor = last . T.splitOn "/vendor/"
