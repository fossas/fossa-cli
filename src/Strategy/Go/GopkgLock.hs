module Strategy.Go.GopkgLock
  ( discover
  , strategy
  , analyze
  , configure

  , Project(..)

  , buildGraph
  )
  where

import Prologue hiding ((.=))

import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Toml (TomlCodec, (.=))
import qualified Toml

import           Diagnostics
import           Discovery.Walk
import qualified Effect.Error as E
import           Effect.Exec
import           Effect.Graphing
import           Effect.ReadFS
import qualified Graph as G
import           Strategy.Go.Transitive (fillInTransitive)
import           Strategy.Go.Types
import           Types

discover :: Discover
discover = Discover
  { discoverName = "gopkglock"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files ->
  case find (\f -> fileName f == "Gopkg.lock") files of
    Nothing -> walkContinue
    Just file  -> do
      output (configure file)
      walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "golang-gopkglock"
  , strategyAnalyze = analyze
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = NotComplete
  }

golockCodec :: TomlCodec GoLock
golockCodec = GoLock
  <$> Toml.list projectCodec "projects" .= lockProjects

projectCodec :: TomlCodec Project
projectCodec = Project
  <$> Toml.text "name" .= projectName
  <*> Toml.dioptional (Toml.text "source") .= projectSource
  <*> Toml.text "revision" .= projectRevision

newtype GoLock = GoLock
  { lockProjects :: [Project]
  } deriving (Eq, Ord, Show, Generic)

data Project = Project
  { projectName     :: Text
  , projectSource   :: Maybe Text
  , projectRevision :: Text
  } deriving (Eq, Ord, Show, Generic)

analyze :: Members '[Exec, ReadFS, Error ReadFSErr, Error ExecErr] r => BasicFileOpts -> Sem r G.Graph
analyze BasicFileOpts{..} = graphingGolang $ do
  contents <- readContentsText targetFile
  case Toml.decode golockCodec contents of
    Left err -> throw (FileParseError (fromRelFile targetFile) (Toml.prettyException err))
    Right golock -> do
      buildGraph (lockProjects golock)

      -- TODO: logging/etc
      _ <- E.try @ExecErr (fillInTransitive (parent targetFile))
      pure ()

buildGraph :: Member (Graphing GolangPackage) r => [Project] -> Sem r ()
buildGraph = void . traverse_ go
  where
  go :: Member (Graphing GolangPackage) r => Project -> Sem r ()
  go Project{..} = do
    let pkg = mkGolangPackage projectName

    direct pkg
    label pkg (mkGolangVersion projectRevision)

    -- label location when it exists
    traverse_ (label pkg . GolangLabelLocation) projectSource

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
