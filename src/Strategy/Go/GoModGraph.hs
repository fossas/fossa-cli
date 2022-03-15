{-# LANGUAGE RecordWildCards #-}

module Strategy.Go.GoModGraph (
  analyze,

  -- * for testing
  GoGraphMod (..),
  parseGoModGraph,
  parseGoGraphMod,
  buildGraph,
) where

import Control.Effect.Diagnostics
import Control.Monad (void)
import Data.Aeson.Internal (formatError)
import Data.Foldable (find)
import Data.Map qualified as Map
import Data.SemVer qualified as SemVer
import Data.SemVer.Internal (Version (..))
import Data.Set (Set, fromList, member, notMember)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import DepTypes (
  DepType (GoType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  ExecErr (CommandParseError),
  execParser,
  execThrow,
 )
import Graphing qualified
import Path
import Strategy.Go.GoList (GoListModule (GoListModule, isIndirect, isMain, path, version))
import Strategy.Go.Gomod (PackageVersion (..), parsePackageVersion)
import Strategy.Go.Transitive (decodeMany)
import Text.Megaparsec (
  MonadParsec (eof),
  Parsec,
  empty,
  errorBundlePretty,
  optional,
  parse,
  sepEndBy,
  some,
  try,
  (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Types (GraphBreadth (..))

-- * Parsing

type Parser = Parsec Void Text

sc :: Parser ()
sc = Lexer.space (void $ some $ char ' ') empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: Text -> Parser Text
symbol = Lexer.symbol sc

-- * Commands

goModGraphCmd :: Command
goModGraphCmd =
  Command
    { cmdName = "go"
    , cmdArgs = ["mod", "graph"]
    , cmdAllowErr = Never
    }

goListJsonCmd :: Command
goListJsonCmd =
  Command
    { cmdName = "go"
    , cmdArgs = ["list", "-m", "-json", "all"]
    , cmdAllowErr = Never
    }

data GoGraphMod
  = MainMod Text
  | OtherMod Text PackageVersion
  deriving (Show, Eq, Ord)

-- * Parsers

-- | Parses go mode entry from 'go mod graph'.
parseGoGraphMod :: Parser GoGraphMod
parseGoGraphMod = do
  name <- toText <$> lexeme (some (alphaNumChar <|> char '/' <|> char '-' <|> char '.' <|> char '_' <|> char '~'))
  version <- optional $ do
    _ <- try $ symbol "@"
    lexeme (parsePackageVersion lexeme)
  case version of
    Nothing -> pure $ MainMod name
    Just v -> pure $ OtherMod name v

toGoModVersion :: Text -> Maybe PackageVersion
toGoModVersion modVersion = case parse (parsePackageVersion lexeme) "go module version" modVersion of
  Left err -> fail $ errorBundlePretty err
  Right vc -> Just vc

-- | Parses output of 'go mod graph'.
parseGoModGraph :: Parser [(GoGraphMod, GoGraphMod)]
parseGoModGraph = parseGoModPair `sepEndBy` (symbol "\n" <|> symbol "\r") <* eof
  where
    parseGoModPair :: Parser (GoGraphMod, GoGraphMod)
    parseGoModPair = (,) <$> parseGoGraphMod <*> parseGoGraphMod

-- Builds graph from edges, main module, and selected module version set.
buildGraph :: [(GoGraphMod, GoGraphMod)] -> GoGraphMod -> Set GoGraphMod -> Set GoGraphMod -> Bool -> Graphing.Graphing Dependency
buildGraph fromToMods mainMod selectedMods directMods applyMVS =
  Graphing.gmap toDependency
    . Graphing.filter (withSelection)
    . Graphing.promoteToDirect (`member` directMods)
    . Graphing.shrink (/= mainMod)
    . Graphing.edges
    $ fromToMods
  where
    withSelection :: GoGraphMod -> Bool
    withSelection m = not (m `notMember` selectedMods && applyMVS)

    mkDependency :: Text -> Maybe VerConstraint -> Dependency
    mkDependency name version =
      Dependency
        { dependencyType = GoType
        , dependencyName = name
        , dependencyVersion = version
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

    toDependency :: GoGraphMod -> Dependency
    toDependency (MainMod name) = mkDependency name Nothing
    toDependency (OtherMod name pkgVersion) = mkDependency name (Just $ toVersion pkgVersion)

    toVersion :: PackageVersion -> VerConstraint
    toVersion v = case v of
      NonCanonical n -> CEq n
      Pseudo commitHash -> CEq commitHash
      Semantic semver -> CEq ("v" <> SemVer.toText semver{_versionMeta = []})

analyze ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m (Graphing.Graphing Dependency, GraphBreadth)
analyze dir = do
  -- Get selected module version from mvs selection using `go list`
  -- `go list` reports final versions that will be used in a build for all direct and deep dependencies
  goListStdout <- context ("Getting selected dependencies versions using, " <> toText (show goListJsonCmd)) $ execThrow dir goListJsonCmd
  (mainMod, selectedMods, directMods) <- case decodeMany goListStdout of
    Left (path, err) -> fatal (CommandParseError goListJsonCmd (toText (formatError path err)))
    Right (mods :: [GoListModule]) -> pure (onlyMain mods, withoutMain mods, onlyDirects mods)

  -- Command 'go mod graph' reports, all module version considered (not just final version selected)
  -- For this reason, we filter out versions of module, not used in build using (versions provided by 'go list')
  -- Further, we use provided 'Indirect' property to infer if the module is "direct" (provided by 'go list')
  --
  -- By filtering out (not selected mod version), replicate https://golang.org/ref/mod#minimal-version-selection
  -- Reference: https://github.com/golang/exp/tree/master/cmd/modgraphviz
  let filterModsNotUsedInBuild = True
  goModGraphStdout <- context ("Getting selected dependencies versions using, " <> toText (show goModGraphCmd)) $ execParser parseGoModGraph dir goModGraphCmd
  ggm <- fromMaybeText "expected to find main module, but found no main module!" mainMod

  let graph = buildGraph goModGraphStdout ggm selectedMods directMods filterModsNotUsedInBuild
  pure (graph, Complete)
  where
    -- TODO: need to convert the version from the replacement into something well-typed
    toGoGraphMod :: GoListModule -> GoGraphMod
    toGoGraphMod GoListModule{..} = case (toGoModVersion =<< version) of
      Nothing -> MainMod path
      Just pv -> OtherMod path pv

    withoutMain :: [GoListModule] -> Set GoGraphMod
    withoutMain = fromList . map toGoGraphMod . filter (not . isMain)

    onlyDirects :: [GoListModule] -> Set GoGraphMod
    onlyDirects = fromList . map toGoGraphMod . filter (not . isIndirect)

    onlyMain :: [GoListModule] -> Maybe GoGraphMod
    onlyMain mods = toGoGraphMod <$> find isMain mods
