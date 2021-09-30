module Strategy.Go.Types (
  GolangPackage, -- don't export GolangPackage; export the smart constructor instead
  mkGolangPackage,
  GolangGrapher,
  GolangLabel (..),
  mkGolangVersion,
  graphingGolang,
) where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes
import Effect.Grapher
import Graphing

-- | A golang package is uniquely identified by its import path
newtype GolangPackage = GolangPackage {goImportPath :: Text} deriving (Eq, Ord, Show)

-- | Smart constructor for @GolangPackage@. Applies 'unvendor' to the value
mkGolangPackage :: Text -> GolangPackage
mkGolangPackage = GolangPackage . unvendor

type GolangGrapher = LabeledGrapher GolangPackage GolangLabel

data GolangLabel
  = GolangLabelVersion Text
  | GolangLabelLocation Text
  deriving (Eq, Ord, Show)

-- | Smart constructor for GolangLabelVersion. Applies 'fixVersion' to the value
mkGolangVersion :: Text -> GolangLabel
mkGolangVersion = GolangLabelVersion . fixVersion

-- | Monomorphic interpreter for @LabeledGrapher GolangPackage@ into a @Graphing Dependency@
graphingGolang :: Algebra sig m => LabeledGrapherC GolangPackage GolangLabel m a -> m (Graphing Dependency)
graphingGolang = withLabeling golangPackageToDependency

golangPackageToDependency :: GolangPackage -> Set GolangLabel -> Dependency
golangPackageToDependency pkg = foldr applyLabel start
  where
    start :: Dependency
    start =
      Dependency
        { dependencyType = GoType
        , dependencyName = goImportPath pkg
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

    applyLabel :: GolangLabel -> Dependency -> Dependency
    applyLabel (GolangLabelVersion ver) dep = dep{dependencyVersion = Just (CEq ver)}
    applyLabel (GolangLabelLocation loc) dep = dep{dependencyLocations = loc : dependencyLocations dep}

-- Replaces "v0.0.0-20191212000000-abcdef+incompatible" with "abcdef".
--
-- This parses "pseudo-versions" of Go modules into the commit hash of the
-- vendored module. See also:
--
-- - What are pseudo-versions? https://golang.org/ref/mod#pseudo-versions
-- - Why do we need pseudo-versions? https://golang.org/ref/mod#glos-pseudo-version
-- - How does Go resolve import paths into download URLs? https://golang.org/cmd/go/#hdr-Remote_import_paths
--
-- FIXME: This doesn't actually work right. For example, it won't handle
-- versions with pre-releases that are NOT pseudo-versions. That's why we have
-- a proper pseudo-version parser in the `go.mod` analyzer.
--
-- TODO: In `go.mod`, we handle this in the parser. Do we even need to handle
-- this format in other Go analyzers? Can we just remove this code?
fixVersion :: Text -> Text
fixVersion = last . Text.splitOn "-" . Text.replace "+incompatible" ""

-- replace "github.com/A/B/vendor/github.com/X/Y" with "github.com/X/Y"
unvendor :: Text -> Text
unvendor = last . Text.splitOn "/vendor/"
