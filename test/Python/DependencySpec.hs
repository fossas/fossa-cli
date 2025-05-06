module Python.DependencySpec (spec) where

import Test.Hspec
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Map qualified as Map

import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction, EnvTesting, EnvOther),
  DepType (PipType, GitType, URLType, UnresolvedPathType),
  Dependency (..),
  VerConstraint (..),
 )

import Strategy.Python.Dependency (
  PythonDependency(..),
  PythonDependencyType(..),
  PythonDependencySource(..),
  fromPoetryDependencyPyProject,
  toDependency,
  mapCategoryToEnvironment,
  determineEnvironmentFromDirect,
 )

spec :: Spec
spec = do
  describe "Python Dependency Conversion" $ do
    describe "Basic Dependency Conversion" $ do
      it "converts simple version dependencies correctly" $ do
        let pythonDep = PythonDependency 
              { pyDepName = "requests"
              , pyDepType = SimpleVersion "^2.0.0"
              , pyDepEnvironments = Set.singleton EnvProduction
              , pyDepExtras = []
              , pyDepMarkers = Nothing
              , pyDepSource = FromPyProject
              }
        
        let fossa = toDependency pythonDep
        
        dependencyName fossa `shouldBe` "requests"
        dependencyType fossa `shouldBe` PipType
        dependencyVersion fossa `shouldSatisfy` isJust
        dependencyEnvironments fossa `shouldBe` Set.singleton EnvProduction

      it "handles git dependencies correctly" $ do
        let pythonDep = PythonDependency 
              { pyDepName = "mypackage"
              , pyDepType = GitDependency "https://github.com/example/repo.git" (Just "main") Nothing Nothing
              , pyDepEnvironments = Set.singleton EnvDevelopment
              , pyDepExtras = []
              , pyDepMarkers = Nothing
              , pyDepSource = FromPyProject
              }
        
        let fossa = toDependency pythonDep
        
        dependencyType fossa `shouldBe` GitType
        dependencyEnvironments fossa `shouldBe` Set.singleton EnvDevelopment
        dependencyLocations fossa `shouldSatisfy` (not . null)

    describe "Environment Mapping" $ do
      it "maps categories to environments correctly" $ do
        mapCategoryToEnvironment "dev" `shouldBe` EnvDevelopment
        mapCategoryToEnvironment "main" `shouldBe` EnvProduction
        mapCategoryToEnvironment "test" `shouldBe` EnvTesting
        mapCategoryToEnvironment "custom" `shouldBe` EnvOther "custom"
      
      it "determines environment from direct flag correctly" $ do
        determineEnvironmentFromDirect True `shouldBe` Set.singleton EnvProduction
        determineEnvironmentFromDirect False `shouldBe` Set.singleton EnvDevelopment

-- Helper function
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False