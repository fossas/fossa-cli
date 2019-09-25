module Strategy.Npm
  ( discover
  , strategy
  , analyze
  , configure
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy.Optics
import           Data.Foldable hiding (find, fold)
import           Data.Function ((&))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           GHC.Generics
import           Optics
import           Path
import           Polysemy
import           Polysemy.Error
import           System.Exit
import           System.FilePath.Find

import           Config
import           Effect.Exec
import           Effect.GraphBuilder
import           Effect.ReadFS
import qualified Graph as G
import           Strategy

data NpmOpts = NpmOpts
  { npmOptsDir :: !(Path Rel Dir)
  } deriving Show

instance FromJSON NpmOpts where
  parseJSON = withObject "NpmOpts" $ \obj -> NpmOpts <$> obj .: "dir"

instance ToJSON NpmOpts where
  toJSON NpmOpts{..} = object ["dir" .= npmOptsDir]

discover :: (Member (Embed IO) r, Member ReadFS r) => Path Abs Dir -> Sem r [ConfiguredStrategy]
discover basedir = do
  paths <- embed $ find (fileName /~? "node_modules") (fileName ==? "package.json") (toFilePath basedir)
  files <- embed @IO $ traverse (stripProperPrefix basedir <=< parseAbsFile) paths
  embed (print files)
  pure $ map (configure . parent) files

strategy :: Strategy NpmOpts
strategy = Strategy
  { strategyName = "nodejs-npm"
  , strategyAnalyze = \opts -> analyze opts
                             & evalGraphBuilderIO
                             & errorToIOFinal @String
                             & execToIO
                             & embedToFinal @IO
                             & runFinal
  }

analyze :: Members '[Exec, Error String, GraphBuilder] r => NpmOpts -> Sem r ()
analyze NpmOpts{..} = do
  (exitcode, stdout, stderr) <- exec npmOptsDir "npm" ["ls", "--json", "--production"]
  when (exitcode /= ExitSuccess) (throw @String $ "NPM returned an error: " <> stderr ^. unpackedChars)
  case eitherDecode stdout of
    Left err -> throw err -- TODO: better error
    Right a -> buildGraph a

buildGraph :: Member GraphBuilder r => NpmOutput -> Sem r ()
buildGraph top = do
  topLevel <- M.traverseWithKey buildNode (outputDependencies top)
  traverse_ addDirect topLevel

  where

  buildNode :: Member GraphBuilder r => Text -> NpmOutput -> Sem r G.DepRef
  buildNode nodeName nodeOutput = do
    children <- M.traverseWithKey buildNode (outputDependencies nodeOutput)
    parentRef <- addNode $ G.Dependency
      { dependencyType = G.NodeJSType
      , dependencyName = nodeName
      , dependencyVersion = outputVersion nodeOutput
      , dependencyLocations = []
      }
    traverse_ (addEdge parentRef) children
    pure parentRef

configure :: Path Rel Dir -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . NpmOpts

data NpmOutput = NpmOutput
  { outputVersion      :: Maybe Text
  , outputFrom         :: Maybe Text
  , outputResolved     :: Maybe Text
  , outputDependencies :: Map Text NpmOutput
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON NpmOutput where
  parseJSON = withObject "NpmOutput" $ \obj ->
    NpmOutput <$> obj .:? "version"
              <*> obj .:? "from"
              <*> obj .:? "resolved"
              <*> obj .:? "dependencies" .!= M.empty
