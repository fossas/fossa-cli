{-# LANGUAGE CPP #-}

module Effect.ExecSpec (
  spec,
) where

import Data.Either (isLeft)
import Effect.Exec (
  AllowErr (..),
  Command (..),
  exec,
  runExecIO,
 )
import Path.IO (getCurrentDir)
import Test.Hspec (
  Spec,
  describe,
  it,
  runIO,
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
