{-# LANGUAGE RecordWildCards #-}

module Strategy.Go.GoModGraph (
  analyze,

  -- * for testing
  GoGraphMod (..),
  GoModReplacements (..),
  GoBuildGraphCfg (..),
  MVSOpt (..),
  parseGoModGraph,
  parseGoGraphMod,
  buildGraph,
  toVerConstraint,
  toGoModVersion,
) where

import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  fatal,
  fromMaybeText,
 )
import Control.Monad (void)
import Data.Aeson.Types (formatError)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.SemVer qualified as SemVer
import Data.SemVer.Internal (Version (..))
import Data.Set (Set, fromList, member)
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
import Path (Abs, Dir, Path)
import Strategy.Go.GoList (GoListModule (GoListModule, isIndirect, isMain, moduleReplacement, path, version), GoModuleReplacement (GoModuleReplacement, pathReplacement, versionReplacement))
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
buildGraph :: GoBuildGraphCfg -> Graphing.Graphing Dependency
buildGraph GoBuildGraphCfg{..} =
  Graphing.gmap (toDependency . tryApplyReplacement)
    . Graphing.filter withSelection
    . Graphing.promoteToDirect (`member` directMods)
    . Graphing.shrink (/= mainMod)
    . Graphing.edges
    $ goModGraphOutput
  where
    GoModReplacements replacements = modReplacements
    withSelection :: GoGraphMod -> Bool
    withSelection m = case mvsOption of
      NoMVS -> True
      _ -> m `member` selectedMods

    tryApplyReplacement :: GoGraphMod -> GoGraphMod
    tryApplyReplacement m = fromMaybe m (Map.lookup m replacements)

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
    toDependency (OtherMod name pkgVersion) = mkDependency name (Just $ toVerConstraint pkgVersion)

toVerConstraint :: PackageVersion -> VerConstraint
toVerConstraint v = case v of
  NonCanonical n -> CEq n
  Pseudo commitHash -> CEq commitHash
  Semantic semver -> CEq ("v" <> SemVer.toText semver{_versionMeta = []})

newtype ModWithReplacement = ModWithReplacement {unModWithReplacement :: (GoGraphMod, GoGraphMod)}
newtype GoModReplacements = GoModReplacements (Map GoGraphMod GoGraphMod)

data GoBuildGraphCfg = GoBuildGraphCfg
  { selectedMods :: Set GoGraphMod
  , mainMod :: GoGraphMod
  , directMods :: Set GoGraphMod
  , modReplacements :: GoModReplacements
  , goModGraphOutput :: [(GoGraphMod, GoGraphMod)]
  , mvsOption :: MVSOpt
  }

-- |Whether or not to apply minimal version selection
data MVSOpt = NoMVS | ApplyMVS

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
  (selectedMods, mainMod, directMods, replacements) <- case decodeMany goListStdout of
    Left (path, err) -> fatal (CommandParseError goListJsonCmd (toText (formatError path err)))
    Right (mods :: [GoListModule]) -> do
      let (selectedMods, replacements) = findSelectedMods mods -- the MainMod can't have replacements
      mainMod <- fromMaybeText "expected to find main module, but found no main module!" (onlyMain mods)
      pure (selectedMods, mainMod, onlyDirects mods, replacements)

  -- Command 'go mod graph' reports, all module version considered (not just final version selected)
  -- For this reason, we filter out versions of module, not used in build using (versions provided by 'go list')
  -- Further, we use provided 'Indirect' property to infer if the module is "direct" (provided by 'go list')
  --
  -- By filtering out (not selected mod version), replicate https://golang.org/ref/mod#minimal-version-selection
  -- Reference: https://github.com/golang/exp/tree/master/cmd/modgraphviz
  let filterModsNotUsedInBuild = ApplyMVS
  goModGraphStdout <- context ("Getting selected dependencies versions using, " <> toText (show goModGraphCmd)) $ execParser parseGoModGraph dir goModGraphCmd

  let graph =
        buildGraph
          GoBuildGraphCfg
            { selectedMods = selectedMods
            , mainMod = mainMod
            , modReplacements = replacements
            , goModGraphOutput = goModGraphStdout
            , mvsOption = filterModsNotUsedInBuild
            , directMods = directMods
            }
  pure (graph, Complete)
  where
    toGoGraphMod :: GoListModule -> GoGraphMod
    toGoGraphMod GoListModule{..} = case (toGoModVersion =<< version) of
      Nothing -> MainMod path
      Just pv -> OtherMod path pv

    findSelectedMods :: [GoListModule] -> (Set GoGraphMod, GoModReplacements)
    findSelectedMods =
      bimap fromList (mkReplacementMap . catMaybes)
        . unzip
        . map toGraphModWithReplacement
        . filter (not . isMain)

    mkReplacementMap :: [ModWithReplacement] -> GoModReplacements
    mkReplacementMap = GoModReplacements . Map.fromList . map unModWithReplacement

    toGraphModWithReplacement :: GoListModule -> (GoGraphMod, Maybe ModWithReplacement)
    toGraphModWithReplacement listMod =
      case toGoGraphMod listMod of
        m@(MainMod _) -> (m, Nothing)
        m@(OtherMod _ _) ->
          ( m
          , do
              GoModuleReplacement{..} <- moduleReplacement listMod
              replacedVersion <- toGoModVersion versionReplacement
              Just $ ModWithReplacement (m, OtherMod pathReplacement replacedVersion)
          )
    onlyDirects :: [GoListModule] -> Set GoGraphMod
    onlyDirects = fromList . map toGoGraphMod . filter (not . isIndirect)

    onlyMain :: [GoListModule] -> Maybe GoGraphMod
    onlyMain mods = toGoGraphMod <$> find isMain mods
