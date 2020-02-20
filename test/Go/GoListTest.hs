{-# language TemplateHaskell #-}

module Go.GoListTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M

import DepTypes
import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Effect.Exec
import Effect.Grapher
import Graphing (Graphing(..))
import Strategy.Go.GoList
import Types

import Test.Tasty.Hspec

runConstExec :: BL.ByteString -> ConstExecC m a -> m a
runConstExec output = runReader output . runConstExecC

newtype ConstExecC m a = ConstExecC { runConstExecC :: ReaderC (BL.ByteString) m a }
  deriving (Functor, Applicative, Monad)

instance Algebra sig m => Algebra (Exec :+: sig) (ConstExecC m) where
  alg (L (Exec _ _ _ k)) = do
    output <- ConstExecC ask
    k (Right output)
  alg (R other) = ConstExecC (alg (R (handleCoercible other)))

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $ Dependency { dependencyType = GoType
                      , dependencyName = "github.com/pkg/one"
                      , dependencyVersion = Just (CEq "commithash")
                      , dependencyLocations = []
                      , dependencyTags = M.empty
                      }
  direct $ Dependency { dependencyType = GoType
                      , dependencyName = "github.com/pkg/two"
                      , dependencyVersion = Just (CEq "v2.0.0")
                      , dependencyLocations = []
                      , dependencyTags = M.empty
                      }

testdir :: Path Rel Dir
testdir = $(mkRelDir ".")

spec_analyze :: Spec
spec_analyze = do
  outputTrivial <- runIO (BL.readFile "test/Go/testdata/golist-stdout.trivial")
  outputComplex <- runIO (BL.readFile "test/Go/testdata/golist-stdout.complex")

  describe "golist analyze" $ do
    it "produces the expected output" $ do
      let result =
            analyze testdir
              & runConstExec outputTrivial
              & runError @ExecErr
              & run
      case result of
        Left err -> expectationFailure ("analyze failed: " <> show err)
        Right closure -> dependenciesGraph (closureDependencies closure) `shouldBe` expected

    it "can handle complex inputs" $ do
      let result =
            analyze testdir
              & runConstExec outputComplex
              & runError @ExecErr
              & run

      case result of
          Left err -> fail $ "failed to build graph" <> show err
          Right closure -> length (graphingDirect (dependenciesGraph (closureDependencies closure))) `shouldBe` 12
