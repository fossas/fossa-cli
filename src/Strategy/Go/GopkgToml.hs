module Strategy.Go.GopkgToml
  ( discover
  , strategy
  , analyze
  , configure

  , buildGraph
  )
  where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.Output
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Toml

import           Diagnostics
import           Discovery.Walk
import           Effect.ReadFS
import           Effect.GraphBuilder
import qualified Graph as G
import           Types

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
  , strategyModule = parent . targetFile
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

gopkgCodec :: Toml.TomlCodec Gopkg
gopkgCodec = Gopkg
  <$> Toml.list constraintCodec "constraint" Toml..= pkgConstraints
  <*> Toml.list constraintCodec "override" Toml..= pkgOverrides

constraintCodec :: Toml.TomlCodec PkgConstraint
constraintCodec = PkgConstraint
  <$> Toml.text "name" Toml..= constraintName
  <*> Toml.dioptional (Toml.text "version") Toml..= constraintVersion
  <*> Toml.dioptional (Toml.text "branch") Toml..= constraintBranch
  <*> Toml.dioptional (Toml.text "revision") Toml..= constraintRevision

data Gopkg = Gopkg
  { pkgConstraints :: [PkgConstraint]
  , pkgOverrides   :: [PkgConstraint]
  } deriving (Eq, Ord, Show, Generic)

data PkgConstraint = PkgConstraint
  { constraintName     :: Text
  , constraintVersion  :: Maybe Text
  , constraintBranch   :: Maybe Text
  , constraintRevision :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

data PkgOverride = PkgOverride
  deriving (Eq, Ord, Show, Generic)

analyze :: Members '[ReadFS, Error CLIErr] r => BasicFileOpts -> Sem r G.Graph
analyze BasicFileOpts{..} = do
  contents <- readContentsText targetFile
  case Toml.decode gopkgCodec contents of
    Left err -> throw @CLIErr (FileParseError (fromRelFile targetFile) (Toml.prettyException err))
    Right gopkg -> pure (buildGraph gopkg)

buildGraph :: Gopkg -> G.Graph
buildGraph gopkg = unfold direct (const []) toDependency
  where
  direct = undefined
  toDependency = undefined
    --G.Dependency { dependencyType = G.GemType
                 --, dependencyName = depName
                 --, dependencyVersion = Just (G.CEq depVersion)
                 --, dependencyLocations = []
                 --, dependencyTags = M.empty
                 --}

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
