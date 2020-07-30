module Effect.ExecSpec
  ( spec,
  )
where

import Data.Either (isLeft)
import Effect.Exec
import Path.IO (getCurrentDir)
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "IO-based handler" $ do
    it "should return Left, and not throw an exception, when a command is not found" $ do
      dir <- getCurrentDir
      res <- runExecIO $ exec dir (Command "lkajsdflkjasdlfkjas" [] Always)
      res `shouldSatisfy` isLeft
