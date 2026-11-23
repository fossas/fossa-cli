{-# LANGUAGE TemplateHaskell #-}

module NuGet.NuGetSpec (
  spec,
) where

import Control.Effect.Reader (local)
import Discovery.Filters (AllFilters (..), comboExclude)
import Discovery.Simple (cachedDiscoverWith)
import Discovery.Walk (walkWithFilters')
import Path (mkRelDir, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.NuGet (NuGetProject (..))
import Strategy.NuGet qualified as NuGet
import Test.Effect (it', shouldMatchList')
import Test.Hspec (Spec, describe, runIO)
import Types (DiscoveredProject (projectData), DiscoveredProjectType (NuGetProjectType))

spec :: Spec
spec = do
  currDir <- runIO getCurrentDir
  let projectDir = currDir </> $(mkRelDir "test/NuGet/testdata/multi-csproj")
      appCore = projectDir </> $(mkRelFile "App.Core.csproj")
      ingageWeb = projectDir </> $(mkRelFile "IngageWeb.csproj")
      restoredDir = currDir </> $(mkRelDir "test/NuGet/testdata/restored-csproj")
      restoredAssets = restoredDir </> $(mkRelFile "obj/project.assets.json")
      restoredCsproj = restoredDir </> $(mkRelFile "App.csproj")
      excludeObj = AllFilters mempty $ comboExclude [] [$(mkRelDir "obj")]
  describe "NuGet discovery" $ do
    it' "discovers every .csproj sibling in a directory (ANE-2523)" $ do
      projects <- cachedDiscoverWith NuGetProjectType NuGet.postDiscover projectDir (walkWithFilters' NuGet.discoverVisitor projectDir)
      let files = map (nugetProjectFile . projectData) projects
      files `shouldMatchList'` [appCore, ingageWeb]

    it' "skips a .csproj whose obj/project.assets.json exists" $ do
      projects <- cachedDiscoverWith NuGetProjectType NuGet.postDiscover restoredDir (walkWithFilters' NuGet.discoverVisitor restoredDir)
      let files = map (nugetProjectFile . projectData) projects
      files `shouldMatchList'` [restoredAssets]

    it' "falls back to the .csproj when path filters exclude obj/ (ANE-3419)" $ do
      projects <- local (const excludeObj) $ cachedDiscoverWith NuGetProjectType NuGet.postDiscover restoredDir (walkWithFilters' NuGet.discoverVisitor restoredDir)
      let files = map (nugetProjectFile . projectData) projects
      files `shouldMatchList'` [restoredCsproj]
