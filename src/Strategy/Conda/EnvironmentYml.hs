{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Strategy.Conda.EnvironmentYml (
  analyze,
  buildGraph,
  graphBreadth,
  EnvironmentYmlFile (..),
  CondaPkg (..),
  PipEntry (..),
) where

import Control.Applicative ((<|>))
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Foldable (traverse_)
import Data.List.Extra ((!?))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Effect.ReadFS
import Errata (Errata (..))
import Graphing (Graphing, fromList)
import Path
import Strategy.Python.ReqTxt (isNonRequirementLine)
import Strategy.Python.Util (Req, reqToDependency, requirementParser)
import Text.Megaparsec (runParser)
import Types

buildGraph :: EnvironmentYmlFile -> Graphing Dependency
buildGraph envYmlFile = Graphing.fromList (condaPkgToDependency =<< dependencies envYmlFile)
  where
    condaPkgToDependency :: CondaPkg -> [Dependency]
    condaPkgToDependency (Pkg text) = [toDependency . getCondaDepFromText $ text]
    condaPkgToDependency (PipPkg entries) = reqToDependency <$> mapMaybe pipEntryReq entries

    pipEntryReq :: PipEntry -> Maybe Req
    pipEntryReq (PipReq req) = Just req
    pipEntryReq (PipSkipped _) = Nothing
    pipEntryReq (PipUnreadable _) = Nothing

    toDependency :: CondaDep -> Dependency
    toDependency CondaDep{..} =
      Dependency
        { dependencyType = CondaType
        , dependencyName = depName
        , dependencyVersion = CEq <$> depVersion -- todo - properly handle version constraints
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

-- | Static analysis cannot follow a skipped @pip:@ entry (an @-r@ file, a
-- local path, a bare URL) or read an unreadable one, so a graph that dropped
-- any of them is 'Partial'.
graphBreadth :: EnvironmentYmlFile -> GraphBreadth
graphBreadth envYmlFile =
  if any isDropped (pipEntries envYmlFile) then Partial else Complete
  where
    isDropped :: PipEntry -> Bool
    isDropped (PipReq _) = False
    isDropped (PipSkipped _) = True
    isDropped (PipUnreadable _) = True

pipEntries :: EnvironmentYmlFile -> [PipEntry]
pipEntries = concatMap entriesOf . dependencies
  where
    entriesOf :: CondaPkg -> [PipEntry]
    entriesOf (Pkg _) = []
    entriesOf (PipPkg entries) = entries

analyze ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs File ->
  m (Graphing Dependency, GraphBreadth)
analyze envYml = do
  envYmlFile <- readContentsYaml @EnvironmentYmlFile envYml
  traverse_ (warn . UnreadablePipRequirement envYml) (mapMaybe unreadable (pipEntries envYmlFile))
  pure (buildGraph envYmlFile, graphBreadth envYmlFile)
  where
    unreadable :: PipEntry -> Maybe Text
    unreadable (PipUnreadable entry) = Just entry
    unreadable _ = Nothing

-- | A named requirement under @pip:@ that the requirement parser could not
-- read. It is left out of the graph, which is reported as 'Partial'.
data UnreadablePipRequirement = UnreadablePipRequirement (Path Abs File) Text

instance ToDiagnostic UnreadablePipRequirement where
  renderDiagnostic (UnreadablePipRequirement file entry) =
    Errata (Just header) [] (Just body)
    where
      header = "Could not read the pip requirement '" <> entry <> "' in " <> toText (show file)
      body = "It is left out of the dependency graph of this environment.yml."

-- | an example Text: "biopython=1.78=py38haf1e3a3_0"
--   '=' is a delimeter for <name>=<version>=<build>
--   where <version> and <build> are optional
getCondaDepFromText :: Text -> CondaDep
getCondaDepFromText rcd =
  CondaDep
    { depName = name
    , depVersion = version
    , -- Note these aren't currently being used
      depBuild = build
    , depFullVersion = fullVersion
    }
  where
    depSplit = Text.split (== '=') rcd

    name = fromMaybe rcd (depSplit !? 0)
    version = depSplit !? 1 -- TODO: this may contain constraints that we need to parse
    build = depSplit !? 2
    fullVersion = getFullVersion version build

    -- if we have a version AND a build, we combine them to form a full version
    -- >>> getFullVersion (Just "1.2.3") (Just "build")
    -- Just "1.2.3=build"
    getFullVersion :: Maybe Text -> Maybe Text -> Maybe Text
    getFullVersion a b = do
      aVal <- a
      let bVal = case b of
            Just x -> "=" <> x
            Nothing -> ""
      pure (aVal <> bVal)

newtype PackageManager = PackageManager Text
  deriving newtype (Eq, Ord, Show, FromJSON)

-- | One entry of a @pip:@ list.
data PipEntry
  = -- | A requirement the parser reads.
    PipReq Req
  | -- | An entry pip accepts that names no package here: a pip option such as
    -- @-r requirements.txt@, a local path, a bare URL or a comment.
    PipSkipped Text
  | -- | A named requirement the parser cannot read.
    PipUnreadable Text
  deriving (Eq, Ord, Show)

data CondaPkg
  = Pkg Text
  | PipPkg [PipEntry]
  deriving (Eq, Ord, Show)

-- | A valid package for the purpose of conda can be:
--
-- * Just a Conda package specifier, @biopython=1.78=py38haf1e3a3_0@
-- * A pip package, which looks like:
--
-- @
--  pip:
--    - pytest-httpserver==1.0.6
-- @
--
-- Conda hands the @pip:@ list to pip as a requirements file, so it may also hold:
--
-- @
-- pip:
--   - git+https://github.com/blaze/dask.git#egg=dask[complete]
--   - -r requirements.txt
-- @
--
-- A bare URL (one without a @<pkg name> \@@ prefix) names no package, so it is
-- skipped. @-r requirements.txt@ is not followed either: the referenced file is
-- analyzed only when discovery finds it on its own, which it does for a file
-- named @requirements.txt@ but not for, say, @requirements/prod.txt@. Both used
-- to fail the whole environment.yml. See 'pipEntry'.
instance FromJSON CondaPkg where
  parseJSON cd = (Pkg <$> parseJSON cd) <|> withObject "CondaPipPkg" parseOtherPkgManager cd
    where
      parseOtherPkgManager :: Object -> Parser CondaPkg
      parseOtherPkgManager (KeyMap.toList -> [("pip", deps)]) = withArray "Pip Packages" parseEntries deps
      parseOtherPkgManager obj = fail ("Expected object with a single key (\"pip\") and list of deps. Got: " <> show obj)

      parseEntries :: Array -> Parser CondaPkg
      parseEntries = fmap PipPkg . traverse (withText "pip depstring parse" (pure . pipEntry)) . Vector.toList

-- | Classify one @pip:@ entry the way pip reads a requirements.txt line.
-- Lines that name no requirement (pip options such as @-r requirements.txt@,
-- local paths, bare URLs, comments) are skipped, exactly as
-- 'Strategy.Python.ReqTxt.requirementsTxtParser' skips them. Anything else is
-- a named requirement, which either parses or is reported as unreadable.
pipEntry :: Text -> PipEntry
pipEntry raw =
  if Text.null entry || isNonRequirementLine entry
    then PipSkipped raw
    else case runParser requirementParser "" entry of
      Right req -> PipReq req
      Left _ -> PipUnreadable raw
  where
    entry = Text.strip raw

data EnvironmentYmlFile = EnvironmentYmlFile
  { name :: Text
  , dependencies :: [CondaPkg]
  }
  deriving (Eq, Ord, Show)

instance FromJSON EnvironmentYmlFile where
  parseJSON = withObject "EnvironmentYmlFile" $ \obj ->
    EnvironmentYmlFile
      <$> obj .: "name"
      <*> obj .: "dependencies"

data CondaDep = CondaDep
  { depName :: Text
  , depVersion :: Maybe Text
  , depBuild :: Maybe Text
  , depFullVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)
