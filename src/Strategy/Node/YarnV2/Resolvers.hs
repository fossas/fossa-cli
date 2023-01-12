-- | Yarn v2 has a handful of default protocols it supports through "Resolvers"
-- in bundled plugins.
--
-- Protocol documentation can be found here: https://yarnpkg.com/features/protocols
--
-- Confusingly, the package examples described on that page are /Descriptors/,
-- not Locators. As such, we don't care to support parsing all of these
-- examples: we only care to parse the Locators produced by the related
-- Resolvers
--
-- See also: default plugins, many of which contain resolvers https://github.com/yarnpkg/berry/blob/8afcaa2a954e196d6cd997f8ba506f776df83b1f/packages/yarnpkg-cli/package.json#L68-L82
module Strategy.Node.YarnV2.Resolvers (
  -- * Primary exports
  Resolver (..),
  Package (..),
  resolveLocatorToPackage,

  -- * Individual resolvers
  workspaceResolver,
  npmResolver,
  gitResolver,
  tarResolver,
  fileResolver,
  libResolver,
  linkResolver,
  execResolver,
  portalResolver,
  patchResolver,
) where

import Control.Effect.Diagnostics
import Data.Foldable (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Extra (dropPrefix, showT)
import Data.Void (Void)
import Strategy.Node.YarnV2.Lockfile
import Text.Megaparsec

data Resolver = Resolver
  { resolverName :: Text
  -- ^ Used for error messages
  , resolverSupportsLocator :: Locator -> Bool
  -- ^ Does this resolver support the locator?
  , resolverLocatorToPackage :: Locator -> Either Text Package
  -- ^ Convert this locator to a yarn package
  }

-- Default Yarn Protocols can be found at https://yarnpkg.com/features/protocols.
data Package
  = WorkspacePackage Text -- relative reference to a directory. not quite a Path Rel Dir because it may contain '..'
  | NpmPackage (Maybe Text) Text Text -- scope, package, version
  | GitPackage Text Text -- url, commit
  | TarPackage Text -- url
  | FilePackage Text
  -- LibPackages are a custom protocol supported by a user.
  -- TODO: Support all custom protocols out of the box.
  | LibPackage Text
  | LinkPackage Text
  | PortalPackage Text
  | ExecPackage Text
  | PatchPackage Text
  deriving (Eq, Ord, Show)

----------

-- | Search for a resolver that supports the Locator, and turn it into a Package
resolveLocatorToPackage :: Has Diagnostics sig m => Locator -> m Package
resolveLocatorToPackage locator = context ("Resolving locator " <> showT locator) $ do
  resolver <-
    fromMaybe @Text "Unsupported locator (no resolver found)" $
      find (`resolverSupportsLocator` locator) allResolvers

  context ("Running resolver: " <> resolverName resolver) . fromEither $
    resolverLocatorToPackage resolver locator

allResolvers :: [Resolver]
allResolvers =
  [ workspaceResolver
  , npmResolver
  , -- Ensure that tarResolver appears before gitResolver in this list.
    -- Currently there are some package locators that the git resolver matches that are actually tarballs.
    -- To get around this, the tarResolver gets to examine a locator first.
    -- Ideally the git resolver's 'resolverSupportsLocator' function would be more specific.
    -- ANE-720 captures that work.
    tarResolver
  , gitResolver
  , fileResolver
  , libResolver
  , linkResolver
  , execResolver
  , portalResolver
  , patchResolver
  ]

---------- WorkspaceResolver

workspaceProtocol :: Text
workspaceProtocol = "workspace:"

-- | Resolved workspace locators come in the form @workspace:./relative/reference/to/dir@
--
-- Relative references may contain '..', so they're not quite @Path Rel Dir@
--
-- See: https://github.com/yarnpkg/berry/blob/8afcaa2a954e196d6cd997f8ba506f776df83b1f/packages/yarnpkg-core/sources/WorkspaceResolver.ts
workspaceResolver :: Resolver
workspaceResolver =
  Resolver
    { resolverName = "WorkspaceResolver"
    , resolverSupportsLocator = (workspaceProtocol `Text.isPrefixOf`) . locatorReference
    , resolverLocatorToPackage =
        Right
          . WorkspacePackage
          . dropPrefix workspaceProtocol
          . locatorReference
    }

---------- NpmResolver

npmProtocol :: Text
npmProtocol = "npm:"

-- | A resolver for packages that come from npm
--
-- As a fun implementation detail, this resolver is split across several
-- "resolvers" in the yarn codebase -- though by the time locators are committed
-- to a yarn lockfile, they're always structured the same way
--
-- ..with one caveat anyway. npm locators are allowed to contain `::selectors`
-- at the end. This is often used for a pinned archive URL from npm:
--
-- @
--     npm:8.0.0::__archiveUrl=https://....
-- @
--
-- ..so when converting to a package, we drop anything after we find a colon.
--
-- See: https://github.com/yarnpkg/berry/blob/8afcaa2a954e196d6cd997f8ba506f776df83b1f/packages/plugin-npm/tests/NpmSemverResolver.test.ts#L7
-- See: https://github.com/yarnpkg/berry/blob/master/packages/plugin-npm/sources/NpmSemverResolver.ts#L26
-- See: Npm*Resolver in the yarn codebase (and NpmSemverResolver in particular)
npmResolver :: Resolver
npmResolver =
  Resolver
    { resolverName = "NpmResolver"
    , resolverSupportsLocator = (npmProtocol `Text.isPrefixOf`) . locatorReference
    , resolverLocatorToPackage = \loc ->
        Right $ NpmPackage (locatorScope loc) (locatorName loc) (Text.takeWhile (/= ':') (dropPrefix npmProtocol (locatorReference loc)))
    }

---------- GitResolver

-- | The git resolver in yarn ALWAYS normalizes and resolves git references the same way:
--
-- @
--     URL#METADATA
--
--     -- e.g.
--
--     https://github.com/foo/bar.git#commit=$COMMITID
--     https://example.com/baz.git#branch=$BRANCH&commit=$COMMITID
-- @
--
-- The metadata (string after #) is a set of key/value pairs, separated by &
--
-- We can always expect to find a commit key, so we use that for the package
--
-- See: https://github.com/yarnpkg/berry/blob/master/packages/plugin-git/sources/GitResolver.ts
gitResolver :: Resolver
gitResolver =
  Resolver
    { resolverName = "GitResolver"
    , resolverSupportsLocator = ("commit=" `Text.isInfixOf`) . locatorReference
    , resolverLocatorToPackage = gitResolverLocatorToPackage
    }

gitResolverLocatorToPackage :: Locator -> Either Text Package
gitResolverLocatorToPackage loc = do
  (url, metadata) <-
    tag ("Invalid git reference: " <> locatorReference loc) $
      splitSingle "#" (locatorReference loc)

  metaMap <-
    tag ("Failed to parse git metadata: " <> metadata) $
      parseGitMetadata metadata

  commit <-
    tag ("Couldn't find commit in git metadata: " <> showT metaMap) $
      Map.lookup "commit" metaMap

  Right $ GitPackage url commit

tag :: a -> Maybe b -> Either a b
tag a = maybe (Left a) Right

-- | Text.splitOn, but only expects to split once
splitSingle :: Text -> Text -> Maybe (Text, Text)
splitSingle needle txt =
  case Text.splitOn needle txt of
    [a, b] -> pure (a, b)
    _ -> Nothing

-- | Parse a metadata string from a git yarn locator into a Map
--
-- The metadata string is formatted as "key1=foo&key2=bar&key3=baz"
parseGitMetadata :: Text -> Maybe (Map Text Text)
parseGitMetadata = fmap Map.fromList . traverse (splitSingle "=") . Text.splitOn "&"

---------- TarResolver

type Parser = Parsec Void Text

matchParser :: Parser a -> Text -> Bool
matchParser p = either (const False) (const True) . runParser p ""

-- | For a locator to be a valid tar, it must match both of these regexes:
--
-- @
--     export const TARBALL_REGEXP = /^[^?]*\.(?:tar\.gz|tgz)(?:\?.*)?$/;
--     export const PROTOCOL_REGEXP = /^https?:/;
-- @
tarMatchP :: Parser ()
tarMatchP = do
  _ <- chunk "https:" <|> chunk "http:"
  lookForExtension
  _ <- optional (single '?' *> takeRest)
  eof
  where
    lookForExtension = do
      _ <- takeWhileP Nothing (\c -> c /= '?' && c /= '.')
      found <- optional $ chunk ".tar.gz" <|> chunk ".tgz"
      case found of
        -- if takeWhileP stopped at a '.', we can continue. '?' is still disallowed
        Nothing -> single '.' *> lookForExtension
        Just _ -> pure ()

-- | The tar resolver supports http/https URLs that point to a tarball (.tar.gz/.tgz)
--
-- See: https://github.com/yarnpkg/berry/blob/master/packages/plugin-http/sources/TarballHttpResolver.ts
tarResolver :: Resolver
tarResolver =
  Resolver
    { resolverName = "TarResolver"
    , resolverSupportsLocator = matchParser tarMatchP . locatorReference
    , resolverLocatorToPackage = Right . TarPackage . locatorReference
    }

---------- Unsupported (by fossa) resolvers

-- | The file resolver supports local "file:" references on disk
fileResolver :: Resolver
fileResolver = unsupportedResolver "FileResolver" "file:" FilePackage

-- | The link resolver is similar to the file resolver
--
-- FOSSA cannot handle these, so we don't do any further parsing of the
-- resolution field
linkResolver :: Resolver
linkResolver = unsupportedResolver "LinkResolver" "link:" LinkPackage

-- | The portal resolver is similar to the link resolver
--
-- FOSSA cannot handle these, so we don't do any further parsing of the
-- resolution field
portalResolver :: Resolver
portalResolver = unsupportedResolver "PortalResolver" "portal:" PortalPackage

-- | The exec resolver allows you to point to a script to run; the output of the
-- script is used as a package
--
-- FOSSA cannot handle these, so we don't do any further parsing of the
-- resolution field
execResolver :: Resolver
execResolver = unsupportedResolver "ExecResolver" "exec:" ExecPackage

-- | The lib resolver is a custom implementation of the portal protocol
--
-- FOSSA cannot handle these, so we don't do any further parsing of the
-- resolution field
libResolver :: Resolver
libResolver = unsupportedResolver "LibResolver" "lib:" LibPackage

-- | The patch resolver allows you to modify another package with patch files.
-- The packages appear elsewhere in the lockfile, so we don't do any further
-- processing of the resolution field
patchResolver :: Resolver
patchResolver = unsupportedResolver "PatchResolver" "patch:" PatchPackage

unsupportedResolver :: Text -> Text -> (Text -> Package) -> Resolver
unsupportedResolver name protocol constructor =
  Resolver
    { resolverName = name
    , resolverSupportsLocator = (protocol `Text.isPrefixOf`) . locatorReference
    , resolverLocatorToPackage = Right . constructor . dropPrefix protocol . locatorReference
    }
