{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.VSI.DynLinked.Internal.Resolve (
  toSourceUnit,
  toDependency,
  environmentDistro,
  readLinuxDistro,
) where

import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.BinaryDeps (analyzeSingleBinary)
import App.Fossa.VSI.DynLinked.Types (DynamicDependency (..), LinuxDistro (..), LinuxPackageManager (..), LinuxPackageMetadata (..), ResolvedLinuxPackage (..))
import App.Fossa.VSI.DynLinked.Util (fsRoot, runningLinux)
import Control.Algebra (Has)
import Control.Applicative.Permutations (intercalateEffect, toPermutation)
import Control.Effect.Diagnostics (Diagnostics, fatal, recover, (<||>))
import Control.Effect.Lift (Lift)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Set (Set, toList)
import Data.String.Conversion (toText)
import Data.Text (Text, intercalate)
import Data.Text qualified as Text
import Data.Void (Void)
import DepTypes (DepType (LinuxAPK, LinuxDEB, LinuxRPM), Dependency (..), VerConstraint (CEq))
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS, readContentsText)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, Dir, File, Path, mkRelFile, (</>))
import Srclib.Converter qualified as Srclib
import Srclib.Types (AdditionalDepData (..), SourceUnit (..), SourceUserDefDep)
import Text.Megaparsec (Parsec, empty, errorBundlePretty, runParser, takeWhile1P)
import Text.Megaparsec.Char (space1)
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
  let unit = Srclib.toSourceUnit False project
  pure $ unit{additionalData = fmap toDepData (Just binaries)}
  where
    toDepData :: [SourceUserDefDep] -> AdditionalDepData
    toDepData d = AdditionalDepData (Just d) Nothing
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
  | not runningLinux = pure Nothing
  | otherwise = recover $ readOsReleaseAt primaryPath <||> readOsReleaseAt fallbackPath
  where
    primaryPath :: Path Abs File
    primaryPath = fsRoot </> $(mkRelFile "etc/os-release")

    fallbackPath :: Path Abs File
    fallbackPath = fsRoot </> $(mkRelFile "usr/lib/os-release")

    readOsReleaseAt :: (Has Diagnostics sig m, Has ReadFS sig m) => Path Abs File -> m LinuxDistro
    readOsReleaseAt file = do
      content <- readContentsText file
      readLinuxDistro content

-- | Reads the linux distro from a file.
readLinuxDistro :: (Has Diagnostics sig m) => Text -> m LinuxDistro
readLinuxDistro contents = case runParser parseLinuxDistro "" filteredLines of
  Left err -> fatal $ toText (errorBundlePretty err)
  Right a -> pure a
  where
    prefixes :: [Text]
    prefixes = map (<> "=") ["ID", "VERSION_ID"]

    filteredLines :: Text
    filteredLines = filterLinePrefixes contents

    filterLinePrefixes :: Text -> Text
    filterLinePrefixes = Text.unlines . filter lineIsUsed . fmap Text.strip . Text.lines
      where
        lineIsUsed line = any (`Text.isPrefixOf` line) prefixes

type Parser = Parsec Void Text

parseLinuxDistro :: Parser LinuxDistro
parseLinuxDistro =
  intercalateEffect sc $
    LinuxDistro
      <$> toPermutation (parseField "ID")
      <*> toPermutation (parseField "VERSION_ID")

parseField :: Text -> Parser Text
parseField field = sc *> symbol field *> symbol "=" *> ident

-- | Consume spaces.
sc :: Parser ()
sc = L.space space1 empty empty

-- | Run the provided parser, then consume any trailing spaces.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse for the provided symbol, then consume any trailing spaces.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Collect a contiguous list of non-space characters into a @Text@, then consume any trailing spaces.
-- Requires that a space trails the identifier.
ident :: Parser Text
ident = lexeme $ toText <$> takeWhile1P Nothing (not . isSpace)
