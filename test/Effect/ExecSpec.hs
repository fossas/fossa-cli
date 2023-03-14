{-# LANGUAGE CPP #-}

module Effect.ExecSpec (
  spec,
) where

import App.Types (OverrideDynamicAnalysisBinary (..))
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Reader (runReader)
import Control.Carrier.Stack (runStack)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.String.Conversion (toString)
import DepTypes (DepType (MavenType))
import Diag.Result (Result (..))
import Effect.Exec (
  AllowErr (..),
  CandidateAnalysisCommands (..),
  Command (..),
  exec,
  mkAnalysisCommand,
  mkSingleCandidateAnalysisCommand,
  runExecIO,
 )
import Path.IO (getCurrentDir)
import Test.Hspec (
  Spec,
  describe,
  expectationFailure,
  it,
  runIO,
  shouldBe,
  shouldSatisfy,
 )
import Prelude

spec :: Spec
spec = do
  dir <- runIO getCurrentDir

  describe "IO-based handler" $ do
    it "should return Left, and not throw an exception, when a command is not found" $ do
      res <- runExecIO $ exec dir (Command "lkajsdflkjasdlfkjas" [] Always)
      res `shouldSatisfy` isLeft

  describe "Command selection" $ do
    -- Use the unix "false" and "true" commands to test this.
    let candidates = CandidateAnalysisCommands ("false" :| ["true"]) [] Nothing
    res <-
      runIO . runStack . runExecIO . runDiagnostics . runReader (mempty :: OverrideDynamicAnalysisBinary) $
        mkAnalysisCommand candidates dir [] Never

    it "should choose the first successful command" $ case res of
      Failure _ _ -> expectationFailure "could not run command selection"
      Success _ cmd -> cmdName cmd `shouldBe` "true"

  describe "Command selection with override" $ do
    -- Use the unix "false" command by default, but override this deptype to also provide "true".
    let candidates = mkSingleCandidateAnalysisCommand "false" [] (Just MavenType)
    let override = OverrideDynamicAnalysisBinary $ Map.fromList [(MavenType, "true")]
    res <-
      runIO . runStack . runExecIO . runDiagnostics . runReader override $
        mkAnalysisCommand candidates dir [] Never

    it "should choose the first successful command" $ case res of
      Failure _ _ -> expectationFailure "could not run command selection"
      Success _ cmd -> cmdName cmd `shouldBe` "true"

  describe "Command selection with no working option" $ do
    -- Use the unix "false" command to test this.
    let candidates = mkSingleCandidateAnalysisCommand "false" [] Nothing
    res <-
      runIO . runStack . runExecIO . runDiagnostics . runReader (mempty :: OverrideDynamicAnalysisBinary) $
        mkAnalysisCommand candidates dir [] Never

    it "should respond with an error" $ case res of
      Failure _ _ -> pure ()
      Success _ cmd -> expectationFailure ("erroneously selected " <> toString (cmdName cmd))
