module Python.PyProjectGenericSpec (
  spec,
) where

import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Reader (Reader)
import Control.Monad.IO.Class (liftIO)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Maybe (isJust, fromJust, isNothing, fromMaybe)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (PipType, URLType, GitType, UnresolvedPathType),
  Dependency (..),
  VerConstraint (..),
 )
import Effect.ReadFS (ReadFS)
import Path (Dir, Path, Rel, fromRelDir, mkRelDir, mkRelFile, (</>))
import Strategy.Python.PyProjectGeneric (
  extractDependencies,
  extractPoetryDependencies,
  extractPDMDependencies,
  extractPEP621Dependencies,
  parseVersionConstraint,
  parseGitDependency,
  parseUrlDependency,
  parsePathDependency,
  parseComplexDependency,
 )
import Strategy.Python.PyProjectGeneric.Types (
  PyProjectGeneric (..),
  PyProjectType (..),
  detectProjectType,
 )
import Strategy.Python.Util (Operator(..))
import Test.Hspec
import Toml qualified

spec :: Spec
spec = do
  poetryContents <- runIO (TIO.readFile "test/Python/PyProjectGeneric/testdata/poetry/pyproject.toml")
  pdmContents <- runIO (TIO.readFile "test/Python/PyProjectGeneric/testdata/pdm/pyproject.toml")
  pep621Contents <- runIO (TIO.readFile "test/Python/PyProjectGeneric/testdata/pep621/pyproject.toml")
  
  -- Load complex dependency test files
  poetryComplexContents <- runIO (TIO.readFile "test/Python/PyProjectGeneric/testdata/complex_deps/poetry.toml")
  pdmComplexContents <- runIO (TIO.readFile "test/Python/PyProjectGeneric/testdata/complex_deps/pdm.toml")
  pep621ComplexContents <- runIO (TIO.readFile "test/Python/PyProjectGeneric/testdata/complex_deps/pep621.toml")

  describe "PyProjectGeneric" $ do
    describe "Project Type Detection" $ do
      it "detects Poetry project" $ do
        putStrLn $ "Poetry contents: " ++ show poetryContents
        case Toml.decode poetryContents of
          Toml.Success _ pyproject -> do
            -- Debug info to understand what's happening
            let poetryInfo = poetrySection pyproject
            putStrLn $ "Poetry section: " ++ show poetryInfo
            putStrLn $ "PDM section: " ++ show (pdmSection pyproject)
            putStrLn $ "Project type: " ++ show (projectType pyproject)
            projectType pyproject `shouldBe` PoetryProject
          Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs

      it "detects PDM project" $ do
        case Toml.decode pdmContents of
          Toml.Success _ pyproject -> projectType pyproject `shouldBe` PDMProject
          Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs

      it "detects PEP 621 standard project" $ do
        case Toml.decode pep621Contents of
          Toml.Success _ pyproject -> projectType pyproject `shouldBe` PEP621Project
          Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs


    describe "PyProject.toml Parsing" $ do
      it "successfully parses a Poetry project" $ do
        case Toml.decode poetryContents of
          Toml.Success _ pyproject -> isJust (poetrySection pyproject) `shouldBe` True
          Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs

      it "successfully parses a PDM project" $ do
        case Toml.decode pdmContents of
          Toml.Success _ pyproject -> isJust (pdmSection pyproject) `shouldBe` True
          Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs

      it "successfully parses a PEP 621 project" $ do
        case Toml.decode pep621Contents of
          Toml.Success _ pyproject -> isJust (projectMetadata pyproject) `shouldBe` True
          Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs

    
    describe "Dependency Extraction" $ do
      describe "Poetry Dependency Extraction" $ do
        it "extracts dependencies from Poetry format" $ do
          case Toml.decode poetryContents of
            Toml.Success _ pyproject -> do
              let deps = extractPoetryDependencies pyproject
              length deps `shouldNotBe` 0
              any (\d -> dependencyName d == "requests") deps `shouldBe` True
              any (\d -> dependencyName d == "django") deps `shouldBe` True
              any (\d -> dependencyName d == "pytest") deps `shouldBe` True
            Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs
        
        it "correctly identifies Poetry production dependencies" $ do
          case Toml.decode poetryContents of
            Toml.Success _ pyproject -> do
              let deps = extractPoetryDependencies pyproject
              let prodDeps = filter (hasEnv EnvProduction) deps
              any (\d -> dependencyName d == "requests") prodDeps `shouldBe` True
              any (\d -> dependencyName d == "django") prodDeps `shouldBe` True
              any (\d -> dependencyName d == "python") prodDeps `shouldBe` True
            Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs
        
        it "correctly identifies Poetry development dependencies" $ do
          case Toml.decode poetryContents of
            Toml.Success _ pyproject -> do
              let deps = extractPoetryDependencies pyproject
              let devDeps = filter (hasEnv EnvDevelopment) deps
              any (\d -> dependencyName d == "pytest") devDeps `shouldBe` True
              any (\d -> dependencyName d == "black") devDeps `shouldBe` True
              any (\d -> dependencyName d == "mypy") devDeps `shouldBe` True
            Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs
        
        it "handles Poetry URL and path dependencies" $ do
          case Toml.decode poetryContents of
            Toml.Success _ pyproject -> do
              let deps = extractPoetryDependencies pyproject
              any (\d -> dependencyName d == "custom" && dependencyType d == GitType) deps `shouldBe` True
              any (\d -> dependencyName d == "pathlib" && dependencyType d == UnresolvedPathType) deps `shouldBe` True
            Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs
      
      describe "PDM Dependency Extraction" $ do
        it "extracts dependencies from PDM format" $ do
          case Toml.decode pdmContents of
            Toml.Success _ pyproject -> do
              let deps = extractPDMDependencies pyproject
              length deps `shouldNotBe` 0
              any (\d -> dependencyName d == "requests") deps `shouldBe` True
              any (\d -> dependencyName d == "flask") deps `shouldBe` True
              any (\d -> dependencyName d == "sqlalchemy") deps `shouldBe` True
              any (\d -> dependencyName d == "pytest") deps `shouldBe` True
            Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs
        
        it "correctly identifies PDM production dependencies" $ do
          case Toml.decode pdmContents of
            Toml.Success _ pyproject -> do
              let deps = extractPDMDependencies pyproject
              let prodDeps = filter (hasEnv EnvProduction) deps
              any (\d -> dependencyName d == "requests") prodDeps `shouldBe` True
              any (\d -> dependencyName d == "flask") prodDeps `shouldBe` True
              any (\d -> dependencyName d == "sqlalchemy") prodDeps `shouldBe` True
            Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs
        
        it "correctly identifies PDM development dependencies" $ do
          case Toml.decode pdmContents of
            Toml.Success _ pyproject -> do
              let deps = extractPDMDependencies pyproject
              let devDeps = filter (hasEnv EnvDevelopment) deps
              any (\d -> dependencyName d == "pytest") devDeps `shouldBe` True
              any (\d -> dependencyName d == "flake8") devDeps `shouldBe` True
              any (\d -> dependencyName d == "pylint") devDeps `shouldBe` True
            Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs
      
      describe "PEP 621 Dependency Extraction" $ do
        it "extracts dependencies from PEP 621 format" $ do
          case Toml.decode pep621Contents of
            Toml.Success _ pyproject -> do
              let deps = extractPEP621Dependencies pyproject
              length deps `shouldNotBe` 0
              any (\d -> dependencyName d == "requests") deps `shouldBe` True
              any (\d -> dependencyName d == "pyyaml") deps `shouldBe` True
              any (\d -> dependencyName d == "urllib3") deps `shouldBe` True
              any (\d -> dependencyName d == "pytest") deps `shouldBe` True
            Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs
        
        it "correctly identifies PEP 621 production dependencies" $ do
          case Toml.decode pep621Contents of
            Toml.Success _ pyproject -> do
              let deps = extractPEP621Dependencies pyproject
              let prodDeps = filter (hasEnv EnvProduction) deps
              any (\d -> dependencyName d == "requests") prodDeps `shouldBe` True
              any (\d -> dependencyName d == "pyyaml") prodDeps `shouldBe` True
              any (\d -> dependencyName d == "urllib3") prodDeps `shouldBe` True
              any (\d -> dependencyName d == "certifi") prodDeps `shouldBe` True
            Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs
        
        it "correctly identifies PEP 621 optional dependencies" $ do
          case Toml.decode pep621Contents of
            Toml.Success _ pyproject -> do
              let deps = extractPEP621Dependencies pyproject
              let devDeps = filter (hasEnv EnvDevelopment) deps
              any (\d -> dependencyName d == "pytest") devDeps `shouldBe` True
              any (\d -> dependencyName d == "black") devDeps `shouldBe` True
              any (\d -> dependencyName d == "sphinx") devDeps `shouldBe` True
              any (\d -> dependencyName d == "pytest-cov") devDeps `shouldBe` True
            Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs
      
      
      describe "Dependency Specification Parsing" $ do
        describe "Helper Functions" $ do
          describe "Version Constraint Parsing" $ do
            it "parses exact versions" $ do
              parseVersionConstraint "==1.2.3" `shouldSatisfy` isJust
              parseVersionConstraint "1.2.3" `shouldSatisfy` isJust
            
            it "parses version ranges" $ do
              parseVersionConstraint ">=1.0.0,<2.0.0" `shouldSatisfy` isJust
            
            it "parses caret requirements" $ do
              parseVersionConstraint "^1.2.3" `shouldSatisfy` isJust
            
            it "parses tilde requirements" $ do
              parseVersionConstraint "~=1.2.3" `shouldSatisfy` isJust
              
            it "parses wildcard version" $ do
              parseVersionConstraint "*" `shouldSatisfy` isJust
          
          describe "Git Dependency Parsing" $ do
            it "parses basic Git URL dependencies" $ do
              let gitDep = parseGitDependency "git+https://github.com/example/repo.git"
              gitDep `shouldSatisfy` isJust
              fmap dependencyType gitDep `shouldBe` Just GitType
            
            it "parses Git dependencies with references" $ do
              let gitDepRef = parseGitDependency "git+https://github.com/example/repo.git@v1.0.0"
              gitDepRef `shouldSatisfy` isJust
              fmap dependencyType gitDepRef `shouldBe` Just GitType
              fmap (not . null . dependencyLocations) gitDepRef `shouldBe` Just True
          
          describe "URL Dependency Parsing" $ do
            it "parses URL dependencies" $ do
              let urlDep = parseUrlDependency "https://example.com/package-1.0.0.tar.gz"
              urlDep `shouldSatisfy` isJust
              fmap dependencyType urlDep `shouldBe` Just URLType
          
          describe "Path Dependency Parsing" $ do
            it "parses path dependencies" $ do
              let pathDep = parsePathDependency "file:../local/package/"
              pathDep `shouldSatisfy` isJust
              fmap dependencyType pathDep `shouldBe` Just UnresolvedPathType
              
              let relPathDep = parsePathDependency "../local/package/"
              relPathDep `shouldSatisfy` isJust
              fmap dependencyType relPathDep `shouldBe` Just UnresolvedPathType
        
        describe "Complex Dependency Parsing" $ do
          it "extracts Poetry complex dependencies" $ do
            case Toml.decode poetryComplexContents of
              Toml.Success _ pyproject -> do
                let deps = extractPoetryDependencies pyproject
                
                -- Check version constraints
                let exactDep = find (\d -> dependencyName d == "exact") deps
                exactDep `shouldSatisfy` isJust
                
                -- Check git dependencies
                let gitSimpleDep = find (\d -> dependencyName d == "git-simple") deps
                gitSimpleDep `shouldSatisfy` isJust
                fmap dependencyType gitSimpleDep `shouldBe` Just GitType
                
                let gitTagDep = find (\d -> dependencyName d == "git-tag") deps
                gitTagDep `shouldSatisfy` isJust
                
                -- Check URL and path dependencies
                let urlDep = find (\d -> dependencyName d == "url") deps
                urlDep `shouldSatisfy` isJust
                fmap dependencyType urlDep `shouldBe` Just URLType
                
                let pathDep = find (\d -> dependencyName d == "path") deps
                pathDep `shouldSatisfy` isJust
                fmap dependencyType pathDep `shouldBe` Just UnresolvedPathType
              
              Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs
              
          it "extracts PDM complex dependencies" $ do
            case Toml.decode pdmComplexContents of
              Toml.Success _ pyproject -> do
                let deps = extractPDMDependencies pyproject
                
                -- Check version constraints
                let rangeDep = find (\d -> dependencyName d == "range") deps
                rangeDep `shouldSatisfy` isJust
                
                -- Check for exact version dependency
                let exactDep = find (\d -> dependencyName d == "exact") deps
                exactDep `shouldSatisfy` isJust
                
              Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs
              
          it "extracts PEP 621 complex dependencies" $ do
            case Toml.decode pep621ComplexContents of
              Toml.Success _ pyproject -> do
                let deps = extractPEP621Dependencies pyproject
                
                -- Check for various dependency types
                any (\d -> dependencyName d == "exact") deps `shouldBe` True
                any (\d -> dependencyName d == "range") deps `shouldBe` True
                
                -- Check test dependencies from optional-dependencies
                let testDeps = filter (hasEnv EnvDevelopment) deps
                length testDeps `shouldNotBe` 0
                
              Toml.Failure errs -> expectationFailure $ "Parse error: " ++ show errs

-- | Helper function to check if a dependency has a specific environment
hasEnv :: DepEnvironment -> Dependency -> Bool
hasEnv env dep = env `Set.member` dependencyEnvironments dep

-- | Helper function to find a dependency by name
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x:xs) = if f x then Just x else find f xs

-- | Helper function to get the head of a list safely
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x