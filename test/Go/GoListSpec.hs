{-# language TemplateHaskell #-}

module Go.GoListSpec
  ( spec
  ) where

import Prologue

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M

import DepTypes
import Control.Algebra
import Control.Carrier.Diagnostics
import Control.Carrier.Reader
import Effect.Exec
import Effect.Grapher
import Graphing (Graphing(..))
import Strategy.Go.GoList
import Types

import Test.Hspec

runConstExec :: BL.ByteString -> ConstExecC m a -> m a
runConstExec output = runReader output . runConstExecC

newtype ConstExecC m a = ConstExecC { runConstExecC :: ReaderC (BL.ByteString) m a }
  deriving (Functor, Applicative, Monad)

instance Algebra sig m => Algebra (Exec :+: sig) (ConstExecC m) where
  alg hdl sig ctx = ConstExecC $ case sig of
    R other -> alg (runConstExecC . hdl) (R other) ctx
    L (Exec _ _) -> do
      output <- ask
      pure (Right output <$ ctx)

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $ Dependency { dependencyType = GoType
                      , dependencyName = "github.com/pkg/one"
                      , dependencyVersion = Just (CEq "commithash")
                      , dependencyLocations = []
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      }
  direct $ Dependency { dependencyType = GoType
                      , dependencyName = "github.com/pkg/two"
                      , dependencyVersion = Just (CEq "v2.0.0")
                      , dependencyLocations = []
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      }

testdir :: Path Rel Dir
testdir = $(mkRelDir ".")

spec :: Spec
spec = do
  outputTrivial <- runIO (BL.readFile "test/Go/testdata/golist-stdout.trivial")
  outputComplex <- runIO (BL.readFile "test/Go/testdata/golist-stdout.complex")

  describe "golist analyze" $ do
    it "produces the expected output" $ do
      let result =
            analyze testdir
              & runConstExec outputTrivial
              & runDiagnostics
              & run
      case result of
        Left err -> expectationFailure ("analyze failed: " <> show (renderFailureBundle err))
        Right body -> dependenciesGraph (bodyDependencies (resultValue body)) `shouldBe` expected

    it "can handle complex inputs" $ do
      let result =
            analyze testdir
              & runConstExec outputComplex
              & runDiagnostics
              & run

      case result of
          Left err -> fail $ "failed to build graph" <> show (renderFailureBundle err)
          Right body -> length (graphingDirect (dependenciesGraph (bodyDependencies (resultValue body)))) `shouldBe` 12
