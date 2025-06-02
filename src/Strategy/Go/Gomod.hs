{-# LANGUAGE RecordWildCards #-}

-- | This module parses go.mod files, used by Go Modules. Go modules were
-- introduced in Go 1.11, and are now on by default in Go 1.16.
--
-- For documentation, see https://golang.org/ref/mod.
module Strategy.Go.Gomod (
  analyze',
  buildGraph,
  Gomod (..),
  Statement (..),
  Require (..),
  PackageName,
  PackageVersion (..),
  parsePackageVersion,
  gomodParser,
  analyzeStatic,
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, recover, warnOnErr)
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.Functor (void, ($>))
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.SemVer qualified as SemVer
import Data.SemVer.Internal (Identifier (..), Version (..))
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import DepTypes (Dependency)
import Diag.Common (
  MissingDeepDeps (MissingDeepDeps),
  MissingEdges (MissingEdges),
 )
import Effect.Exec (Exec)
import Effect.Grapher (direct, label)
import Effect.ReadFS (ReadFS, readContentsParser)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, File, Path, parent)
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types (
  GolangGrapher,
  GolangLabel (..),
  graphingGolang,
  mkGolangPackage,
 )
import Text.Megaparsec (
  MonadParsec (eof, takeWhile1P, try),
  Parsec,
  between,
  chunk,
  count,
  many,
  oneOf,
  optional,
  parse,
  sepBy,
  some,
  (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char, numberChar, space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Types (GraphBreadth (..))

-- For the file's grammar, see https://golang.org/ref/mod#go-mod-file-grammar.
data Statement
  = -- | package, version
    RequireStatement PackageName PackageVersion
  | -- | old, new, newVersion
    ReplaceStatement PackageName PackageName PackageVersion
  | -- | old, dir (local "submodule" dependency -- dir can be a resolvable string (e.g., "../foo") or an actual directory (e.g., "/foo" or "foo/"))
    LocalReplaceStatement PackageName Text
  | -- | package, version
    ExcludeStatement PackageName PackageVersion
  | -- | we do not care about values
    -- associated with retract block for dependency graphing,
    -- as they do not signify any relationship with dependencies
    -- of go.mod, refer to: https://go.dev/ref/mod#go-mod-file-retract
    RetractStatement
  | GoVersionStatement Text
  | -- | we do not care about values associated with
    -- the toolchain block as they are of no use to us today.
    -- Refer to: https://go.dev/doc/modules/gomod-ref#toolchain
    ToolchainStatement Text
  | -- | dependencies in the tool block are development tools
    -- which we do not currently support scanning, so we skip this.
    -- Refer to: https://tip.golang.org/doc/modules/managing-dependencies#tools
    ToolStatement Text
  | -- | Specifies the default GODEBUG settings.
    -- Refer to: https://go.dev/doc/modules/gomod-ref#godebug
    GoDebugStatements Text
  deriving (Eq, Ord, Show)

type PackageName = Text

-- See https://golang.org/ref/mod#go-mod-file-ident. Reproduced below:
--
-- > Versions in go.mod files may be canonical or non-canonical.
-- >
-- > A canonical version starts with the letter v, followed by a semantic
-- > version following the Semantic Versioning 2.0.0 specification. See Versions
-- > for more information.
--
-- Regarding pseudo-versions, see https://golang.org/ref/mod#pseudo-versions.
--
-- > A pseudo-version is a specially formatted pre-release version that encodes
-- > information about a specific revision in a version control repository. For
-- > example, v0.0.0-20191109021931-daa7c04131f5 is a pseudo-version.
data PackageVersion
  = NonCanonical Text -- Something like "master"
  | Pseudo Text
  | Semantic Version
  deriving (Eq, Ord, Show, Generic)

instance Hashable PackageVersion

data Gomod = Gomod
  { modName :: PackageName
  , modRequires :: [Require]
  , modReplaces :: Map PackageName Require
  , modLocalReplaces :: Map PackageName Text
  , modExcludes :: [Require]
  }
  deriving (Eq, Ord, Show)

data Require = Require
  { reqPackage :: PackageName
  , reqVersion :: PackageVersion
  }
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

-- | Parses package version of go module.
-- Version parses version strings and distinguishes between non-canonical
-- versions, semantic versions, and pseudo-versions.
--
-- We first attempt to parse the version as semantic. On success, we
-- determine whether the semantic version is a pseudo-version (which
-- requires special formatting for the backend).
--
-- On failure, we treat the version string as a non-canonical version as
-- long as it's composed of legal characters.
--
-- For how Go modules use versions, see https://golang.org/ref/mod#versions.
-- For the semantic version spec, see https://semver.org/.
parsePackageVersion :: (Parser [Char] -> Parser [Char]) -> Parser PackageVersion
parsePackageVersion lexify = parseSemOrPseudo <|> parseNonCanonical
  where
    -- Helpers.
    semVerText = toText <$> lexify (some (alphaNumChar <|> oneOf ['.', '-', '+', '_', '/']))
    mapLeft _ (Right r) = r
    mapLeft f (Left l) = f l

    -- Non-canonical versions can be made of any valid string of characters.
    parseNonCanonical = NonCanonical <$> semVerText

    -- Pseudo-versions are also valid semantic versions. We first determine
    -- whether a version is properly semantic, and then determine whether it
    -- could also be a pseudo-version.
    parseSemOrPseudo = do
      -- Go modules adds a leading "v". See
      -- https://golang.org/ref/mod#versions.
      --
      -- > Each version starts with the letter v, followed by a semantic
      -- > version.
      _ <- char 'v'
      raw <- semVerText

      -- Once we have the version text, we then try to parse the version
      -- text as a semver using SemVer.fromText.
      pure $ case SemVer.fromText raw of
        Right semver -> case reverse $ _versionRelease semver of
          -- See https://golang.org/ref/mod#pseudo-versions for the forms
          -- of pseudo-versions, reproduced below:
          --
          -- - Form 1 (no tagged version): vX.0.0-yyyymmddhhmmss-abcdefabcdef
          -- - Form 2 (pre-release version): vX.Y.Z-pre.0.yyyymmddhhmmss-abcdefabcdef
          -- - Form 3 (tagged version): vX.Y.(Z+1)-0.yyyymmddhhmmss-abcdefabcdef
          --
          -- Pseudo-versions always have a trailing "pseudo" pre-release
          -- identifier, so we reverse the pre-release identifier list to
          -- match on the end of the list. If the original tagged version
          -- has a pre-release identifier, then the pseudo-version will
          -- separate the original identifier and the "pseudo" identifier
          -- with a numeric identifier section equal to 0.
          --
          -- If a semantic version matches a possible pseudo-version form,
          -- we then check whether the pre-release identifiers are formatted
          -- as if the version is a pseudo-version. If the parse fails, then
          -- we treat this version as a normal semantic version.
          [IText preReleaseId] -> mapLeft (const $ Semantic semver) $ parsePseudoPreRelease preReleaseId
          (IText preReleaseId) : (INum 0) : _ -> mapLeft (const $ Semantic semver) $ parsePseudoPreRelease preReleaseId
          -- If the semantic version's pre-release identifiers are not of a
          -- pseudo-version form, it cannot possibly be a pseudo-version.
          _ -> Semantic semver
        Left _ -> NonCanonical raw

    -- Here, we run a sub-parser to determine whether the "pseudo"
    -- pre-release identifier is correctly formatted. This is always of the
    -- form "yyyymmddhhmmss-abcdefabcdef" - a timestamp, and then a commit
    -- hash.
    --
    -- For the backend's sake, we want to upload the commit hash (which
    -- identifies the version of the dependency).
    --
    -- See https://golang.org/ref/mod#pseudo-versions for more details.
    parsePseudoPreRelease = parse parser ""
      where
        parser :: Parser PackageVersion
        parser = Pseudo <$ count 14 numberChar <* char '-' <*> (toText <$> count 12 alphaNumChar) <* eof

gomodParser :: Parser Gomod
gomodParser = do
  _ <- scn
  _ <- lexeme (chunk "module")
  name <- modulePath
  _ <- scn
  statements <- many (statement <* scn)
  eof

  let statements' = concat statements

  pure (toGomod name statements')
  where
    statement =
      (singleton <$> goDebugStatements) -- singleton wraps the Parser Statement into a Parser [Statement]
        <|> (singleton <$> toolChainStatements)
        <|> (singleton <$> toolStatements)
        <|> (singleton <$> goVersionStatement)
        <|> requireStatements
        <|> replaceStatements
        <|> excludeStatements
        <|> retractStatements

    -- top-level go version statement
    -- e.g., go 1.12
    goVersionStatement :: Parser Statement
    goVersionStatement = GoVersionStatement <$ lexeme (chunk "go") <*> goVersion

    -- top-level toolchain statement
    -- e.g., toolchain go1.21.1
    toolChainStatements :: Parser Statement
    toolChainStatements = ToolchainStatement <$ lexeme (chunk "toolchain") <*> anyToken

    -- top-level tool statement
    -- e.g., tool golang.org/x/tools/cmd/stringer
    toolStatements :: Parser Statement
    toolStatements = ToolStatement <$ lexeme (chunk "tool") <*> anyToken

    -- top-level godebug statement
    -- e.g., godebug asynctimerchan=0
    goDebugStatements :: Parser Statement
    goDebugStatements = GoDebugStatements <$ lexeme (chunk "godebug") <*> anyToken

    -- top-level require statements
    -- e.g.:
    --   require golang.org/x/text v1.0.0
    --   require (
    --       golang.org/x/text v1.0.0
    --       golang.org/x/sync v2.0.0
    --   )
    requireStatements :: Parser [Statement]
    requireStatements = block "require" singleRequire

    -- parse the body of a single require (without the leading "require" lexeme)
    singleRequire = RequireStatement <$> packageName <*> version

    -- top-level replace statements
    -- e.g.:
    --   replace golang.org/x/text => golang.org/x/text v3.0.0
    --   replace (
    --       golang.org/x/sync => golang.org/x/sync v15.0.0
    --       golang.org/x/text => golang.org/x/text v3.0.0
    --   )
    replaceStatements :: Parser [Statement]
    replaceStatements = block "replace" (try singleReplace <|> singleLocalReplace)

    -- parse the body of a single replace (without the leading "replace" lexeme)
    singleReplace :: Parser Statement
    singleReplace = ReplaceStatement <$> packageName <* optional version <* lexeme (chunk "=>") <*> packageName <*> version

    -- We parse "local" replaces differently from normal replaces because we
    -- don't want to upload local file paths as a version to the backend.
    singleLocalReplace :: Parser Statement
    singleLocalReplace = LocalReplaceStatement <$> packageName <* optional version <* lexeme (chunk "=>") <*> anyToken

    -- top-level exclude statements
    -- e.g.:
    --   exclude golang.org/x/text v3.0.0
    --   exclude (
    --       golang.org/x/text v3.0.0
    --       golang.org/x/sync v15.0.0
    --   )
    excludeStatements :: Parser [Statement]
    excludeStatements = block "exclude" singleExclude

    -- parse the body of a single exclude (without the leading "exclude" lexeme)
    singleExclude :: Parser Statement
    singleExclude = ExcludeStatement <$> packageName <*> version

    -- top-level retract statements
    -- e.g.:
    --  retract v1.0.0
    --  retract [v1.0.0, v1.9.9]
    --  retract (
    --    v1.0.0
    --    [v1.0.0, v1.9.9]
    --  )
    retractStatements :: Parser [Statement]
    retractStatements = block "retract" (singleRetract <|> multipleRetract)

    singleRetract :: Parser Statement
    singleRetract = version $> RetractStatement

    multipleRetract :: Parser Statement
    multipleRetract = squareBrackets (version `sepBy` (symbol ",")) $> RetractStatement

    version :: Parser PackageVersion
    version = parsePackageVersion lexeme

    -- helper combinator to parse things like:
    --
    --   prefix <singleparse>
    --
    -- or
    --
    --   prefix (
    --       <singleparse>
    --       <singleparse>
    --       <singleparse>
    --   )
    block prefix parseSingle = do
      _ <- lexeme (chunk prefix)
      parens (many (parseSingle <* scn)) <|> (singleton <$> parseSingle)

    -- package name, e.g., golang.org/x/text
    packageName :: Parser PackageName
    packageName = toText <$> lexeme (some (alphaNumChar <|> char '.' <|> char '/' <|> char '-' <|> char '_'))

    modulePath :: Parser Text
    modulePath =
      packageName
        <|> between (char '"') (char '"') packageName

    -- goVersion, e.g.:
    --   v0.0.0-20190101000000-abcdefabcdef
    --   v1.2.3
    goVersion :: Parser Text
    goVersion = toText <$> lexeme (some (alphaNumChar <|> oneOf ['.', '-', '+']))

    -- singleton list. semantically more meaningful than 'pure'
    singleton :: a -> [a]
    singleton = pure

    -- lexer combinators
    parens = between (symbol "(") (symbol ")")
    squareBrackets = between (symbol "[") (symbol "]")
    symbol = L.symbol scn
    lexeme = L.lexeme sc

    anyToken :: Parser Text
    anyToken = lexeme (takeWhile1P (Just "any token") (not . isSpace))

    -- space consumer WITHOUT newlines (for use with Text.Megaparsec.Char.Lexer combinators)
    sc :: Parser ()
    sc = L.space (void $ some (char ' ' <|> char '\t')) (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

    -- space consumer with newlines
    scn :: Parser ()
    scn = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

toGomod :: Text -> [Statement] -> Gomod
toGomod name = foldr apply (Gomod name [] Map.empty Map.empty [])
  where
    apply (RequireStatement package version) gomod = gomod{modRequires = Require package version : modRequires gomod}
    apply (ReplaceStatement old new newVersion) gomod = gomod{modReplaces = Map.insert old (Require new newVersion) (modReplaces gomod)}
    apply (LocalReplaceStatement old path) gomod = gomod{modLocalReplaces = Map.insert old path (modLocalReplaces gomod)}
    apply (ExcludeStatement package version) gomod = gomod{modExcludes = Require package version : modExcludes gomod}
    apply _ gomod = gomod

-- lookup modRequires and replace them with modReplaces as appropriate, producing the resolved list of requires
resolve :: Gomod -> [Require]
resolve gomod = map resolveReplace . filter nonLocalPackage $ modRequires gomod
  where
    -- nonLocalPackage determines whether the package name is used in a "local
    -- replace" statement -- i.e., a replace statement pointing to a filepath as a
    -- local module
    nonLocalPackage :: Require -> Bool
    nonLocalPackage = not . (`elem` Map.keys (modLocalReplaces gomod)) . reqPackage

    resolveReplace require = fromMaybe require (Map.lookup (reqPackage require) (modReplaces gomod))

analyze' ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs File ->
  m (Graphing Dependency, GraphBreadth)
analyze' file = do
  graph <- graphingGolang $ do
    gomod <- readContentsParser gomodParser file

    context "Building dependency graph" $ buildGraph gomod
    void
      . recover
      . warnOnErr MissingDeepDeps
      . warnOnErr MissingEdges
      $ fillInTransitive (parent file)
    pure ()
  pure (graph, Partial)

-- | This variant of analyze will not attempt to fill in transitive dependencies.
analyzeStatic :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency, GraphBreadth)
analyzeStatic file = do
  graph <- graphingGolang $ do
    gomod <- readContentsParser gomodParser file
    context "Building dependency graph (static)" $ buildGraph gomod
  pure (graph, Partial)

buildGraph :: Has GolangGrapher sig m => Gomod -> m ()
buildGraph = traverse_ go . resolve
  where
    go :: Has GolangGrapher sig m => Require -> m ()
    go Require{..} = do
      let pkg = mkGolangPackage reqPackage

      direct pkg

      -- When labelling the dependency version, we use:
      --
      -- 1. The commit hash for pseudo-versions. TODO: we should augment the
      --    backend to accept full pseudo-versions.
      -- 2. The full semantic version without build metadata identifiers for
      --    semantic versions. This is specifically intended to strip the
      --    "+incompatible" metadata tag that the Go tooling uses. See
      --    https://golang.org/ref/mod#incompatible-versions for details.
      -- 3. The raw version text for non-canonical versions. Nothing else we can
      --    do here.
      label pkg $
        GolangLabelVersion $
          case reqVersion of
            NonCanonical n -> n
            Pseudo commitHash -> commitHash
            Semantic semver -> "v" <> SemVer.toText semver{_versionMeta = []}
