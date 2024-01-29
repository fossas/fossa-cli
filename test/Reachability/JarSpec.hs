{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Reachability.JarSpec (spec) where

import App.Fossa.Reachability.Jar (callGraphFromJar)
import Control.Effect.Lift (sendIO)

import Path (Abs, File, Path, mkRelFile, (</>))
import Path.IO qualified as PIO
import Test.Effect
import Test.Hspec (Spec, describe)

malformedJarFile :: IO (Path Abs File)
malformedJarFile = do
  cwd <- PIO.getCurrentDir
  pure (cwd </> $(mkRelFile "test/Reachability/testdata/malformed.jar"))

spec :: Spec
spec = describe "callGraphFromJar" $ do
  it' "should get not get call graph from corruped jar file" $ do
    jarFile <- sendIO malformedJarFile
    resp <- callGraphFromJar jarFile

    resp `shouldBe'` Nothing
