module Strategy.Go.GopkgToml
  ( discover
  , strategy
  --, analyze
  , configure

  , Gopkg(..)
  , PkgConstraint(..)

  , analyze
  , buildGraph
  )
  where

import Prologue hiding ((.=), empty)

import qualified Data.Map.Strict as M
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Toml (TomlCodec, (.=))
import qualified Toml

import DepTypes
import Diagnostics
import Discovery.Walk
import qualified Effect.Error as E
import Effect.Exec
import Effect.LabeledGrapher
import Effect.ReadFS
import Graphing (Graphing)
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types
import Types

discover :: Discover
discover = Discover
  { discoverName = "gopkgtoml"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files ->
  case find (\f -> fileName f == "Gopkg.toml") files of
    Nothing -> walkContinue
    Just file  -> do
      output (configure file)
      walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "golang-gopkgtoml"
  , strategyAnalyze = analyze
  , strategyLicense = const (pure [])
  , strategyModule = parent . targetFile
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

gopkgCodec :: TomlCodec Gopkg
gopkgCodec = Gopkg
  <$> Toml.list constraintCodec "constraint" .= pkgConstraints
  <*> Toml.list constraintCodec "override" .= pkgOverrides

constraintCodec :: TomlCodec PkgConstraint
constraintCodec = PkgConstraint
  <$> Toml.text "name" .= constraintName
  <*> Toml.dioptional (Toml.text "source") .= constraintSource
  <*> Toml.dioptional (Toml.text "version") .= constraintVersion
  <*> Toml.dioptional (Toml.text "branch") .= constraintBranch
  <*> Toml.dioptional (Toml.text "revision") .= constraintRevision

data Gopkg = Gopkg
  { pkgConstraints :: [PkgConstraint]
  , pkgOverrides   :: [PkgConstraint]
  } deriving (Eq, Ord, Show, Generic)

data PkgConstraint = PkgConstraint
  { constraintName     :: Text
  , constraintSource   :: Maybe Text
  , constraintVersion  :: Maybe Text
  , constraintBranch   :: Maybe Text
  , constraintRevision :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

analyze :: Members '[ReadFS, Exec, Error ReadFSErr, Error ExecErr] r => BasicFileOpts -> Sem r (Graphing Dependency)
analyze BasicFileOpts{..} = graphingGolang $ do
  contents <- readContentsText targetFile
  case Toml.decode gopkgCodec contents of
    Left err -> throw (FileParseError (fromRelFile targetFile) (Toml.prettyException err))
    Right gopkg -> do
      buildGraph gopkg

      -- TODO: logging/etc
      _ <- E.try @ExecErr (fillInTransitive (parent targetFile))
      pure ()

buildGraph :: Member (LabeledGrapher GolangPackage) r => Gopkg -> Sem r ()
buildGraph = void . M.traverseWithKey go . resolve
  where
  go :: Member (LabeledGrapher GolangPackage) r => Text -> PkgConstraint -> Sem r ()
  go name PkgConstraint{..} = do
    let pkg = mkGolangPackage name

    direct pkg

    -- label version when it exists
    traverse_ (label pkg . mkGolangVersion)
              (constraintVersion <|> constraintBranch <|> constraintRevision)

    -- label location when it exists
    traverse_ (label pkg . GolangLabelLocation) constraintSource

-- TODO: handling version constraints
resolve :: Gopkg -> Map Text PkgConstraint -- Map Package (Maybe Version)
resolve gopkg = overridden
  where
  overridden = foldr inserting constraints (pkgOverrides gopkg)
  constraints = foldr inserting M.empty (pkgConstraints gopkg)

  inserting :: PkgConstraint -> Map Text PkgConstraint -> Map Text PkgConstraint
  inserting constraint = M.insert (constraintName constraint) constraint

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
