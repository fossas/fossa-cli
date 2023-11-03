{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.VSI.DynLinked.Internal.Resolve (
  toSourceUnit,
  toDependency,
  environmentDistro,
  parseLinuxDistro,
) where

import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.BinaryDeps (analyzeSingleBinary)
import App.Fossa.VSI.DynLinked.Types (DynamicDependency (..), LinuxDistro (..), LinuxPackageManager (..), LinuxPackageMetadata (..), ResolvedLinuxPackage (..))
import App.Fossa.VSI.DynLinked.Util (fsRoot, runningLinux)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, (<||>))
import Control.Effect.Lift (Lift)
import Data.Either (partitionEithers)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set, toList)
import Data.String.Conversion (toText)
import Data.Text (Text, intercalate)
import Data.Text qualified as Text
import Data.Void (Void)
import DepTypes (DepType (LinuxAPK, LinuxDEB, LinuxRPM), Dependency (..), VerConstraint (CEq))
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS, readContentsParser)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, Dir, File, Path, mkRelFile, (</>))
import Srclib.Converter qualified as Srclib
import Srclib.Types (AdditionalDepData (..), SourceUnit (..), SourceUserDefDep, srcUserDepDescription)
import Text.Megaparsec (Parsec, empty, eof, many, takeWhile1P)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Types (DiscoveredProjectType (VsiProjectType), GraphBreadth (Complete))

-- | Resolves a set of dynamic dependencies into a @SourceUnit@.
-- Any @DynamicDependency@ that isn't resolved to a Linux package dependency is converted to an unknown binary dependency.
toSourceUnit ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  LinuxDistro ->
  Set DynamicDependency ->
  m SourceUnit
toSourceUnit root distro dependencies = do
  let (resolved, unresolved) = sortResolvedUnresolved dependencies
  binaries <- traverse (analyzeSingleBinary root) unresolved

  let project = toProject root . Graphing.directs $ map (toDependency distro) resolved
  let unit = Srclib.projectToSourceUnit False project
  pure $ unit{additionalData = fmap toDepData (Just binaries)}
  where
    toDepData :: [SourceUserDefDep] -> AdditionalDepData
    toDepData d = AdditionalDepData (Just $ fmap updatedDesc d) Nothing
    updatedDesc :: SourceUserDefDep -> SourceUserDefDep
    updatedDesc d = d{srcUserDepDescription = Just "Unmanaged dynamically linked dependency"}
    toProject :: Path Abs Dir -> Graphing Dependency -> ProjectResult
    toProject dir graph = ProjectResult VsiProjectType dir graph Complete []

toDependency :: LinuxDistro -> ResolvedLinuxPackage -> Dependency
toDependency distro ResolvedLinuxPackage{..} = case resolvedLinuxPackageManager of
  LinuxPackageManagerDEB -> renderDEB resolvedLinuxPackageMetadata distro
  LinuxPackageManagerRPM -> renderRPM resolvedLinuxPackageMetadata distro
  LinuxPackageManagerAPK -> renderAPK resolvedLinuxPackageMetadata distro
  where
    render :: DepType -> [Text] -> [Text] -> Dependency
    render fetcher projectParts revisionParts =
      Dependency
        { dependencyType = fetcher
        , dependencyName = fromParts projectParts
        , dependencyVersion = Just . CEq $ fromParts revisionParts
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = mempty
        }

    fromParts :: [Text] -> Text
    fromParts = intercalate "#"

    renderDEB :: LinuxPackageMetadata -> LinuxDistro -> Dependency
    renderDEB LinuxPackageMetadata{..} LinuxDistro{..} =
      render
        LinuxDEB
        [linuxPackageID, linuxDistroName, linuxDistroRelease]
        [linuxPackageArch, linuxPackageRevision]

    renderRPM :: LinuxPackageMetadata -> LinuxDistro -> Dependency
    renderRPM LinuxPackageMetadata{..} LinuxDistro{..} =
      render
        LinuxRPM
        [linuxPackageID, linuxDistroName, linuxDistroRelease]
        [linuxPackageArch, epoch <> linuxPackageRevision]

    renderAPK :: LinuxPackageMetadata -> LinuxDistro -> Dependency
    renderAPK LinuxPackageMetadata{..} LinuxDistro{..} =
      render
        LinuxAPK
        [linuxPackageID, linuxDistroName, linuxDistroRelease]
        [linuxPackageArch, linuxPackageRevision]

    epoch :: Text
    epoch = maybe "" (<> ":") $ linuxPackageDistroEpoch resolvedLinuxPackageMetadata

-- | Sort @Set DynamicDependency@ into two lists: "resolved", and "unresolved", in a single pass.
--
-- Whether a dependency is "resolved" is predicated on whether its @dynamicDependencyResolved@ is not @Nothing@.
-- Dependencies thus sorted are unwrapped into more specific types based on the needs of downstream functions
-- converting this set of dependencies into a @SourceUnit@.
sortResolvedUnresolved :: Set DynamicDependency -> ([ResolvedLinuxPackage], [Path Abs File])
sortResolvedUnresolved = partitionEithers . map forkEither . toList
  where
    forkEither :: DynamicDependency -> Either ResolvedLinuxPackage (Path Abs File)
    forkEither dep = case dynamicDependencyResolved dep of
      Nothing -> Right $ dynamicDependencyDiskPath dep
      Just linuxPackage -> Left linuxPackage

-- | Discover the linux distro under which we are currently executing.
--
-- Evaluates to @Nothing@ on non-Linux environments.
-- Exits via @Diagnostics@ on parse errors.
environmentDistro :: (Has Diagnostics sig m, Has ReadFS sig m) => m (Maybe LinuxDistro)
environmentDistro
  | runningLinux = Just <$> (readOsReleaseAt primaryPath <||> readOsReleaseAt fallbackPath)
  | otherwise = pure Nothing
  where
    primaryPath :: Path Abs File
    primaryPath = fsRoot </> $(mkRelFile "etc/os-release")

    fallbackPath :: Path Abs File
    fallbackPath = fsRoot </> $(mkRelFile "usr/lib/os-release")

    readOsReleaseAt :: (Has Diagnostics sig m, Has ReadFS sig m) => Path Abs File -> m LinuxDistro
    readOsReleaseAt = readContentsParser parseLinuxDistro

stripSurrounding :: Text -> Text -> Text
stripSurrounding strip text = do
  let withoutPrefix = fromMaybe text $ Text.stripPrefix strip text
  fromMaybe withoutPrefix $ Text.stripSuffix strip withoutPrefix

type Parser = Parsec Void Text

parseLinuxDistro :: Parser LinuxDistro
parseLinuxDistro = do
  keys <- Map.fromList <$> many parseField <* eof
  case (Map.lookup "ID" keys, Map.lookup "VERSION_ID" keys) of
    (Just distro, Just version) -> pure $ LinuxDistro (stripSurrounding "\"" distro) (stripSurrounding "\"" version)
    (Just _, Nothing) -> fail "missing required key: VERSION_ID"
    (Nothing, Just _) -> fail "missing required key: ID"
    (Nothing, Nothing) -> fail "missing required keys: ID, VERSION_ID"

parseField :: Parser (Text, Text)
parseField = do
  name <- lexeme $ takeWhile1P Nothing (/= '=') <* char '='
  value <- lexeme $ toText <$> takeWhile1P Nothing (/= '\n')
  pure (Text.strip name, Text.strip value)

-- | Consume spaces.
sc :: Parser ()
sc = L.space space1 empty empty

-- | Run the provided parser, then consume any trailing spaces.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
