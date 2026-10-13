{-# LANGUAGE TemplateHaskell #-}

module NuGet.NuGetSpec (
  spec,
) where

import Path (mkRelDir, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.NuGet (NuGetProject (..))
import Strategy.NuGet qualified as NuGet
import Test.Effect (it', shouldMatchList')
import Test.Hspec (Spec, describe, runIO)
import Types (DiscoveredProject (projectData))

spec :: Spec
spec = do
  currDir <- runIO getCurrentDir
  let projectDir = currDir </> $(mkRelDir "test/NuGet/testdata/multi-csproj")
      appCore = projectDir </> $(mkRelFile "App.Core.csproj")
      ingageWeb = projectDir </> $(mkRelFile "IngageWeb.csproj")
      restoredDir = currDir </> $(mkRelDir "test/NuGet/testdata/restored-csproj")
      restoredAssets = restoredDir </> $(mkRelFile "obj/project.assets.json")
  describe "NuGet discovery" $ do
    it' "discovers every .csproj sibling in a directory (ANE-2523)" $ do
      projects <- NuGet.discover projectDir
      let files = map (nugetProjectFile . projectData) projects
      files `shouldMatchList'` [appCore, ingageWeb]

    it' "skips a .csproj whose obj/project.assets.json exists" $ do
      projects <- NuGet.discover restoredDir
      let files = map (nugetProjectFile . projectData) projects
      files `shouldMatchList'` [restoredAssets]
