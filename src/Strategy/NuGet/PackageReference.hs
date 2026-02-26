{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.PackageReference (
  buildGraph,
  analyze',
  PackageReference (..),
  ItemGroup (..),
  Package (..),
) where

import Control.Applicative (optional, (<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepType (NuGetType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.ReadFS (ReadFS, readContentsXML)
import Graphing (Graphing)
import Graphing qualified
import Parse.XML (FromXML (..), attr, child, children)
import Path (Abs, File, Path)
import Types (
  DependencyResults (..),
  GraphBreadth (Partial),
 )

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m DependencyResults
analyze' file = do
  ref <- readContentsXML @PackageReference file
  graph <- context "Building dependency graph" $ pure (buildGraph ref)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [file]
      }

newtype PackageReference = PackageReference
  { groups :: [ItemGroup]
  }
  deriving (Eq, Ord, Show)

newtype ItemGroup = ItemGroup
  { dependencies :: [Package]
  }
  deriving (Eq, Ord, Show)

data Package = Package
  { depID :: Text
  , depVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromXML PackageReference where
  parseElement el = PackageReference <$> children "ItemGroup" el

instance FromXML ItemGroup where
  parseElement el = ItemGroup <$> children "PackageReference" el

-- | A "PackageReference" xml tag
--
-- See: https://docs.microsoft.com/en-us/dotnet/core/project-sdk/msbuild-props#packagereference
-- See: https://cloud.google.com/functions/docs/writing/specifying-dependencies-dotnet
instance FromXML Package where
  parseElement el =
    Package
      <$> (attr "Include" el <|> attr "Update" el)
      <*> optional (attr "Version" el <|> child "Version" el)

buildGraph :: PackageReference -> Graphing Dependency
buildGraph project = Graphing.fromList (map toDependency validPackages)
  where
    direct = concatMap dependencies (groups project)
    validPackages = filter (not . hasUnresolvedMSBuildVariable) direct

    toDependency Package{..} =
      Dependency
        { dependencyType = NuGetType
        , dependencyName = depID
        , dependencyVersion = fmap CEq depVersion
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

-- | Check if a package has an unresolved MSBuild variable in its version.
--
-- MSBuild variables appear as:
--   - $(VariableName) - standard MSBuild property syntax
--   - $VariableName$  - legacy/alternate syntax seen in some projects
--
-- Packages with unresolved variables cannot be resolved server-side and
-- should be filtered out to avoid timeout failures.
hasUnresolvedMSBuildVariable :: Package -> Bool
hasUnresolvedMSBuildVariable Package{depVersion = Nothing} = False
hasUnresolvedMSBuildVariable Package{depVersion = Just ver} =
  -- Check for $(VarName) pattern
  ("$(" `Text.isInfixOf` ver && ")" `Text.isInfixOf` ver)
    -- Check for $VarName$ pattern (legacy syntax)
    || (Text.count "$" ver >= 2 && not (Text.null (Text.filter (== '$') ver)))
