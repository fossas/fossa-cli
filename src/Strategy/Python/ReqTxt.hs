
module Strategy.Python.ReqTxt
  ( ReqTxtOpts(..)

  , discover
  , strategy
  , analyze
  , configure
  )
  where

import Prologue hiding ((<?>), many, some)

import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Config
import qualified Graph as G
import           Discovery.Walk
import           Effect.ReadFS
import           Strategy
import           Strategy.Python.Util
import           Types

discover :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "requirements.txt") files of
    Nothing -> walkContinue
    Just file  -> do
      output (configure file)
      walkContinue

data ReqTxtOpts = ReqTxtOpts
  { reqTxtOptsFile :: Path Rel File
  } deriving Show

instance FromJSON ReqTxtOpts where
  parseJSON = withObject "ReqTxtOpts" $ \obj ->
    ReqTxtOpts <$> obj .: "file"

instance ToJSON ReqTxtOpts where
  toJSON ReqTxtOpts{..} = object ["file" .= reqTxtOptsFile]

strategy :: Strategy ReqTxtOpts
strategy = Strategy
  { strategyName = "python-piplist"
  , strategyAnalyze = analyze
  }

analyze :: Members '[Error CLIErr, ReadFS, Embed IO] r => ReqTxtOpts -> Sem r G.Graph
analyze ReqTxtOpts{..} = do
  contents <- readContentsText reqTxtOptsFile
  case runParser requirementsTxtParser "source" contents of
    Left err -> throw $ StrategyFailed $ "failed to parse requirements.txt " <> show err -- TODO: better error
    Right a -> pure $ buildGraph a

type Parser = Parsec Void Text

requirementsTxtParser :: Parser [Req]
requirementsTxtParser = requirementParser `sepBy` newline

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . ReqTxtOpts
