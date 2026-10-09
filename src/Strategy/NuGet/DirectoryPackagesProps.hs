module Strategy.NuGet.DirectoryPackagesProps (
  DirectoryPackagesProps (..),
  PackageVersionGroup (..),
  PackageVersionEntry (..),
  PropertyGroup (..),
  UnresolvedPackageVersions (..),
  findAndParse,
  buildVersionMap,
  resolveVersions,
  expandProperties,
  Segment (..),
  valueParser,
) where

import Control.Applicative (optional, (<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, warn, warnOnErr)
import Control.Monad (guard, unless)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum)
import Data.Either (partitionEithers)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Diag.Common (MissingDeepDeps (MissingDeepDeps))
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Effect.ReadFS (ReadFS, doesFileExist, readContentsXML, resolveFile')
import Errata (Errata (..))
import Parse.XML (FromXML (..), attr, child, children)
import Path (Abs, Dir, File, Path, parent, toFilePath)
import Text.Megaparsec (Parsec, chunk, eof, many, parseMaybe, single, takeWhile1P)

-- | Represents a parsed Directory.Packages.props file.
-- See: https://learn.microsoft.com/en-us/nuget/consume-packages/central-package-management
data DirectoryPackagesProps = DirectoryPackagesProps
  { propertyGroups :: [PropertyGroup]
  , packageVersionGroups :: [PackageVersionGroup]
  }
  deriving (Eq, Ord, Show)

-- | A @\<PropertyGroup\>@. Each child element defines an MSBuild property named
-- after the element, with the element's text as its (unexpanded) value.
-- @Condition@ attributes are ignored, so every property is taken as defined.
newtype PropertyGroup = PropertyGroup
  { properties :: Map Text Text
  }
  deriving (Eq, Ord, Show)

newtype PackageVersionGroup = PackageVersionGroup
  { packageVersions :: [PackageVersionEntry]
  }
  deriving (Eq, Ord, Show)

-- | A single @\<PackageVersion\>@ item. MSBuild allows shapes beyond
-- @\<PackageVersion Include="..." Version="..." /\>@: the @Version@ metadata
-- may appear as a child element instead of an attribute, and items like
-- @\<PackageVersion Remove="..." /\>@ carry no version at all. Entries missing
-- a name or version are kept as 'Nothing' and skipped by 'buildVersionMap'
-- rather than failing the parse of the whole file.
data PackageVersionEntry = PackageVersionEntry
  { pvName :: Maybe Text
  , pvVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromXML DirectoryPackagesProps where
  parseElement el =
    DirectoryPackagesProps
      <$> children "PropertyGroup" el
      <*> children "ItemGroup" el

instance FromXML PropertyGroup where
  parseElement el = PropertyGroup <$> parseElement el

instance FromXML PackageVersionGroup where
  parseElement el = PackageVersionGroup <$> children "PackageVersion" el

instance FromXML PackageVersionEntry where
  parseElement el =
    PackageVersionEntry
      <$> optional (attr "Include" el <|> attr "Update" el)
      <*> optional (attr "Version" el <|> child "Version" el)

-- | Raised when a @\<PackageVersion\>@ version references an MSBuild property
-- that cannot be expanded from the props file alone.
newtype UnresolvedPackageVersions = UnresolvedPackageVersions [(Text, Text)]

instance ToDiagnostic UnresolvedPackageVersions where
  renderDiagnostic (UnresolvedPackageVersions entries) =
    Errata (Just header) [] (Just body)
    where
      header = "Could not resolve MSBuild property references in Directory.Packages.props; these packages are reported without a version:"
      body = Text.unlines [name <> ": " <> version | (name, version) <- entries]

-- | Build a map from (case-folded) package name to version from a parsed
-- Directory.Packages.props. Entries whose version cannot be resolved are
-- omitted; see 'resolveVersions'.
buildVersionMap :: DirectoryPackagesProps -> Map Text Text
buildVersionMap = fst . resolveVersions

-- | Resolve the version of every @\<PackageVersion\>@ entry, expanding MSBuild
-- property references such as @Version="$(SomeVar)"@ from the file's
-- @\<PropertyGroup\>@s. Returns the resolved versions keyed by case-folded
-- package name, plus the (name, raw version) of entries that could not be
-- resolved. Those are left out of the map so the package is reported without a
-- version rather than with the literal @$(...)@ text.
resolveVersions :: DirectoryPackagesProps -> (Map Text Text, [(Text, Text)])
resolveVersions props = (Map.fromList resolved, unresolved)
  where
    propertyMap = buildPropertyMap props
    entries = concatMap (mapMaybe toPair . packageVersions) (packageVersionGroups props)
    toPair pv = (,) <$> pvName pv <*> pvVersion pv
    (unresolved, resolved) = partitionEithers (map resolve entries)
    resolve (name, version) =
      case expandProperties propertyMap version of
        Just expanded -> Right (Text.toCaseFold name, expanded)
        Nothing -> Left (name, version)

-- | Collect properties from every @\<PropertyGroup\>@. MSBuild property names
-- are case-insensitive, so keys are case-folded; a later definition of the same
-- property overrides an earlier one, as in MSBuild evaluation.
buildPropertyMap :: DirectoryPackagesProps -> Map Text Text
buildPropertyMap =
  Map.fromList
    . concatMap (map (first Text.toCaseFold) . Map.toList . properties)
    . propertyGroups

-- | One piece of an MSBuild value: literal text, or a @$(Name)@ property reference.
data Segment
  = Literal Text
  | Ref Text
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

-- | Parse an MSBuild value into its segments.
--
-- Once @$(@ is seen the reference must be a plain property name followed by
-- @)@; the parser does not backtrack to treat it as literal text. So an empty
-- or unterminated reference, or property-function syntax such as
-- @$(Foo.Trim())@ or @$([MSBuild]::...)@, fails the parse. A lone @$@ that does
-- not open a reference is literal.
valueParser :: Parser [Segment]
valueParser = many segment <* eof
  where
    segment :: Parser Segment
    segment =
      Ref <$> (chunk "$(" *> takeWhile1P (Just "property name") isNameChar <* single ')')
        <|> Literal <$> takeWhile1P (Just "literal text") (/= '$')
        <|> Literal <$> chunk "$"

    isNameChar :: Char -> Bool
    isNameChar c = isAlphaNum c || c == '_'

-- | Expand every @$(Name)@ reference in a value. Property values may themselves
-- reference other properties, so expansion recurses, bounded to stop on cycles.
--
-- Returns 'Nothing' when the value does not parse (see 'valueParser'), when a
-- referenced property is not defined in the map, or when expansion is cyclic.
expandProperties :: Map Text Text -> Text -> Maybe Text
expandProperties props = go maxDepth
  where
    maxDepth :: Int
    maxDepth = 10

    go :: Int -> Text -> Maybe Text
    go depth value = do
      guard (depth > 0)
      segments <- parseMaybe valueParser value
      Text.concat <$> traverse (expand depth) segments

    expand :: Int -> Segment -> Maybe Text
    expand _ (Literal text) = Just text
    expand depth (Ref name) = Map.lookup (Text.toCaseFold name) props >>= go (depth - 1)

-- | Search for Directory.Packages.props starting from the given directory,
-- walking up parent directories. If found, parse it and return the version map.
findAndParse ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  Path Abs Dir ->
  m (Map Text Text)
findAndParse dir = warnOnErr MissingDeepDeps $ do
  found <- findPropsFile dir
  case found of
    Nothing -> pure Map.empty
    Just propsFile -> do
      props <- readContentsXML @DirectoryPackagesProps propsFile
      let (versions, unresolved) = resolveVersions props
      unless (null unresolved) $ warn (UnresolvedPackageVersions unresolved)
      pure versions

-- | Walk up from @dir@ looking for Directory.Packages.props.
findPropsFile ::
  (Has ReadFS sig m) =>
  Path Abs Dir ->
  m (Maybe (Path Abs File))
findPropsFile dir = do
  let parentDir = parent dir
  resolved <- resolveFile' dir "Directory.Packages.props"
  case resolved of
    Right file -> do
      exists <- doesFileExist file
      if exists
        then pure (Just file)
        else
          if toFilePath dir == toFilePath parentDir
            then pure Nothing -- reached root
            else findPropsFile parentDir
    Left _ ->
      if toFilePath dir == toFilePath parentDir
        then pure Nothing
        else findPropsFile parentDir
