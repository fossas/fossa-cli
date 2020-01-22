module Strategy.Go.GoList
  ( discover
  , strategy
  , analyze
  , configure

  , Require(..)
  )
  where

import Prologue hiding ((<?>))

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Polysemy
import Polysemy.Error
import Polysemy.Output

import DepTypes
import Diagnostics
import Discovery.Walk
import qualified Effect.Error as E
import Effect.Exec
import Effect.LabeledGrapher
import Graphing (Graphing)
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types
import Types

discover :: Discover
discover = Discover
  { discoverName = "golist"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \dir _ files ->
  case find (\f -> fileName f == "go.mod") files of
    Nothing -> walkContinue
    Just _  -> do
      output (configure dir)
      walkContinue

strategy :: Strategy BasicDirOpts
strategy = Strategy
  { strategyName = "golang-golist"
  , strategyAnalyze = analyze
  , strategyModule = targetDir
  , strategyOptimal = Optimal
  , strategyComplete = NotComplete
  }

data Require = Require
  { reqPackage :: Text
  , reqVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

golistCmd :: Command
golistCmd = Command
  { cmdNames = ["go"]
  , cmdBaseArgs = ["list", "-m", "all"]
  , cmdAllowErr = Never
  }

analyze :: Members '[Error ExecErr, Exec] r => BasicDirOpts -> Sem r (Graphing Dependency)
analyze BasicDirOpts{..} = graphingGolang $ do
  stdout <- execThrow targetDir golistCmd []

  let gomodLines = drop 1 . T.lines . T.filter (/= '\r') . decodeUtf8 . BL.toStrict $ stdout -- the first line is our package
      requires = mapMaybe toRequire gomodLines

      toRequire :: Text -> Maybe Require
      toRequire line =
        case T.splitOn " " line of
          [package, version] -> Just (Require package version)
          _ -> Nothing

  buildGraph requires

  -- TODO: logging/etc
  _ <- E.try @ExecErr (fillInTransitive targetDir)
  pure ()

buildGraph :: Member (LabeledGrapher GolangPackage) r => [Require] -> Sem r ()
buildGraph = traverse_ go
  where

  go :: Member (LabeledGrapher GolangPackage) r => Require -> Sem r ()
  go Require{..} = do
    let pkg = mkGolangPackage reqPackage
    direct pkg
    label pkg (mkGolangVersion reqVersion)

configure :: Path Rel Dir -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicDirOpts
