
{-# language QuasiQuotes #-}

module MyLib
  ( app
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char (toLower)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Function
import           Data.Maybe
import           Data.Monoid (Last)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as M
import           GHC.Generics (Generic)
import           Path
import           Path.IO
import           Polysemy
import           Polysemy.Error

import Config
import Discovery
import Effect.ReadFS
import Strategy
import qualified Strategy.Npm as Npm

discovery :: Members '[Embed IO, Error DiscoverErr, ReadFS] r => Sem r [ConfiguredStrategy]
discovery = do
  dir <- getCurrentDir
  concat <$> sequence [Npm.discover dir, loadConfig strategiesByName dir]

-- TODO: newtypes?
type ModuleType = String
type StrategyName = String

moduleTypes :: Map ModuleType [StrategyName]
moduleTypes = M.fromList
  [ ("nodejs", ["nodejs-npm"])
  ]

strategiesByName :: Map String SomeStrategy
strategiesByName = M.fromList (map (\strategy@(SomeStrategy Strategy{strategyName}) -> (strategyName, strategy)) strategies)

strategies :: [SomeStrategy]
strategies = [SomeStrategy Npm.strategy]

app :: IO ()
app = do
  setCurrentDir [absdir|/Users/connor/.go/src/github.com/fossas/fossa-cli/|]
  !discovered <- discovery & readFSToIO
                          & errorToIOFinal @DiscoverErr
                          & embedToFinal @IO
                          & runFinal
  case discovered of
    Left err -> print err
    --Right a -> print (length a)
    --Right a -> traverse (\(ConfiguredStrategy strat opt) -> print (encode opt)) a *> pure ()
    Right a -> print =<< traverse (\(ConfiguredStrategy strat opt) -> strategyAnalyze strat opt) a -- $ map(\(ConfiguredStrategy strat opt) -> BS.pack (strategyName strat) <> BL.toStrict (encode opt)) a
