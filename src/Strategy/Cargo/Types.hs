module Strategy.Cargo.Types (
  PackageId (..),
  PackageDependency (..),
  Package (..),
  NodeDepKind (..),
  NodeDependency (..),
  ResolveNode (..),
  Resolve (..),
  CargoMetadata (..),
  parsePkgId,
) where

import Control.Applicative ((<|>))
import Data.Aeson.Types (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Bifunctor (first)
import Data.Functor (void)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, errorBundlePretty, lookAhead, optional, parse, takeRest, takeWhile1P, try)
import Text.Megaparsec.Char (char, digitChar, space, string)

data PackageId = PackageId
  { pkgIdName :: Text
  , pkgIdVersion :: Text
  , pkgIdSource :: Text
  }
  deriving (Eq, Ord, Show)

data PackageDependency = PackageDependency
  { pkgDepName :: Text
  , pkgDepReq :: Text
  , pkgDepKind :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data Package = Package
  { pkgName :: Text
  , pkgVersion :: Text
  , pkgId :: PackageId
  , pkgLicense :: Maybe Text
  , pkgLicenseFile :: Maybe Text
  , pkgDependencies :: [PackageDependency]
  , pkgSourceUrl :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data NodeDepKind = NodeDepKind
  { nodeDepKind :: Maybe Text
  , nodeDepTarget :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data NodeDependency = NodeDependency
  { nodePkg :: PackageId
  , nodeDepKinds :: [NodeDepKind]
  }
  deriving (Eq, Ord, Show)

data ResolveNode = ResolveNode
  { resolveNodeId :: PackageId
  , resolveNodeDeps :: [NodeDependency]
  }
  deriving (Eq, Ord, Show)

newtype Resolve = Resolve
  { resolvedNodes :: [ResolveNode]
  }
  deriving (Eq, Ord, Show)

data CargoMetadata = CargoMetadata
  { metadataPackages :: [Package]
  , metadataWorkspaceMembers :: [PackageId]
  , metadataResolve :: Resolve
  }
  deriving (Eq, Ord, Show)

instance FromJSON PackageDependency where
  parseJSON = withObject "PackageDependency" $ \obj ->
    PackageDependency
      <$> obj .: "name"
      <*> obj .: "req"
      <*> obj .:? "kind"

instance FromJSON Package where
  parseJSON = withObject "Package" $ \obj ->
    Package
      <$> obj .: "name"
      <*> obj .: "version"
      <*> (obj .: "id" >>= parsePkgId)
      <*> obj .:? "license"
      <*> obj .:? "license_file"
      <*> obj .: "dependencies"
      <*> obj .:? "source"

instance FromJSON NodeDepKind where
  parseJSON = withObject "NodeDepKind" $ \obj ->
    NodeDepKind
      <$> obj .:? "kind"
      <*> obj .:? "target"

instance FromJSON NodeDependency where
  parseJSON = withObject "NodeDependency" $ \obj ->
    NodeDependency
      <$> (obj .: "pkg" >>= parsePkgId)
      <*> obj .: "dep_kinds"

instance FromJSON ResolveNode where
  parseJSON = withObject "ResolveNode" $ \obj ->
    ResolveNode
      <$> (obj .: "id" >>= parsePkgId)
      <*> obj .: "deps"

instance FromJSON Resolve where
  parseJSON = withObject "Resolve" $ \obj ->
    Resolve <$> obj .: "nodes"

instance FromJSON CargoMetadata where
  parseJSON = withObject "CargoMetadata" $ \obj ->
    CargoMetadata
      <$> obj .: "packages"
      <*> (obj .: "workspace_members" >>= traverse parsePkgId)
      <*> obj .: "resolve"

-- | Custom Parsec type alias
type PkgSpecParser a = Parsec Void Text a

-- | Parser for pre cargo v1.77.0 package ids.
oldPkgIdParser :: PkgSpecParser PackageId
oldPkgIdParser = do
  name <- takeWhile1P (Just "Package name") (/= ' ') <* char ' '
  version <- takeWhile1P (Just "Package version") (/= ' ') <* string " ("
  source <- takeWhile1P (Just "Package source") (/= ')')
  pure $
    PackageId
      { pkgIdName = name
      , pkgIdVersion = version
      , pkgIdSource = source
      }

type PkgName = Text
type PkgVersion = Text

-- | Parser for post cargo v1.77.0 package ids
newPkgIdParser :: PkgSpecParser PackageId
newPkgIdParser = eatSpaces (try longSpec <|> simplePkgSpec')
  where
    eatSpaces m = space *> m <* space

    -- Given the fragment: adler@1.0.2
    pkgName :: PkgSpecParser (PkgName, PkgVersion)
    pkgName = do
      -- Parse: adler
      name <- takeWhile1P (Just "Package name") (`notElem` ['@', ':'])
      -- Parse: @1.0.2
      version <- optional (choice [char '@', char ':'] *> semver)
      -- It's possible to specify a name with no version, use "*" in this case.
      pure (name, fromMaybe "*" version)

    simplePkgSpec' =
      pkgName >>= \(name, version) ->
        pure
          PackageId
            { pkgIdName = name
            , pkgIdVersion = version
            , pkgIdSource = ""
            }

    -- Given the spec: registry+https://github.com/rust-lang/crates.io-index#adler@1.0.2
    longSpec :: PkgSpecParser PackageId
    longSpec = do
      -- Parse: registry+https
      sourceInit <- takeWhile1P (Just "Initial URL") (/= ':')
      -- Parse: ://github.com/rust-lang/crates.io-index
      sourceRemaining <- takeWhile1P (Just "Remaining URL") (/= '#')
      let pkgSource = sourceInit <> sourceRemaining

      -- In cases where we can't find a real name, use text after the last slash as a name.
      -- e.g. file:///path/to/my/project/bar#2.0.0 has the name 'bar'
      -- Cases of this are generally path dependencies.
      -- Strip query parameters (e.g. ?tag=v0.3.6) before splitting, so that
      -- git+https://github.com/fossas/broker?tag=v0.3.6#0.3.6 yields "broker", not "broker?tag=v0.3.6".
      let fallbackName =
            maybe pkgSource NonEmpty.last
              . NonEmpty.nonEmpty
              . filter (/= "")
              . Text.split (== '/')
              $ Text.takeWhile (/= '?') sourceRemaining

      -- Parse (Optional): #adler@1.0.2
      nameVersion <- optional $ do
        void $ char '#'
        -- If there's only a version after '#', use the fallback as the name.
        ((fallbackName,) <$> semver)
          <|> pkgName

      let (name, version) = fromMaybe (fallbackName, "*") nameVersion
      pure $
        PackageId
          { pkgIdName = name
          , pkgIdVersion = version
          , pkgIdSource = pkgSource
          }

    -- In the grammar, a semver always appears at the end of a string and is the only
    -- non-terminal that starts with a digit, so don't bother parsing internally.
    semver = try (lookAhead digitChar) *> takeRest

-- Prior to Cargo 1.77.0, package IDs looked like this:
-- package version (source URL)
-- adler 1.0.2 (registry+https://github.com/rust-lang/crates.io-index)
--
-- For 1.77.0 and later, they look like this:
-- registry source URL with a fragment of package@version
-- registry+https://github.com/rust-lang/crates.io-index#adler@1.0.2
-- or
-- path source URL with a fragment of package@version
-- path+file:///Users/scott/projects/health-data/health_data#package_name@0.1.0
-- or
-- path source URL with a fragment of version
-- In this case we grab the last entry in the path to use for the package name
-- path+file:///Users/scott/projects/health-data/health_data#0.1.0
--
-- Package Spec: https://doc.rust-lang.org/cargo/reference/pkgid-spec.html
parsePkgId :: MonadFail m => Text -> m PackageId
parsePkgId t = either fail pure $ first errorBundlePretty parseResult
  where
    parseResult = parse (try oldPkgIdParser <|> newPkgIdParser) "Cargo package spec" t
