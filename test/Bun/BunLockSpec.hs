{-# LANGUAGE TemplateHaskell #-}

module Bun.BunLockSpec (
  spec,
) where

import Data.Aeson (eitherDecodeStrict)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion qualified
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
 )
import GraphUtil (
  expectDirect,
  expectEdge,
 )
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, File, Path, fromAbsFile, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Node.Bun.BunLock (
  BunLockFile (..),
  BunPackage (..),
  BunWorkspace (..),
  buildGraph,
  parseBunLock,
 )
import Test.Effect (it', shouldBe')
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO, shouldBe)

spec :: Spec
spec = do
  currentDir <- runIO getCurrentDir
  let simpleBunLockPath = currentDir </> $(mkRelFile "test/Bun/testdata/simple-bun.lock")
  let workspaceBunLockPath = currentDir </> $(mkRelFile "test/Bun/testdata/workspace-bun.lock")
  let devdepsBunLockPath = currentDir </> $(mkRelFile "test/Bun/testdata/devdeps-bun.lock")
  let trailingCommasBunLockPath = currentDir </> $(mkRelFile "test/Bun/testdata/trailing-commas-bun.lock")

  describe "bun.lock simple" $ do
    it' "should parse simple bun.lock" $ do
      lockFile <- parseBunLock simpleBunLockPath
      lockfileVersion lockFile `shouldBe'` 1

      -- Check workspaces
      let ws = workspaces lockFile
      Map.size ws `shouldBe'` 1
      let rootWs = ws Map.! ""
      name rootWs `shouldBe'` "simple-project"
      dependencies rootWs `shouldBe'` Map.fromList [("lodash", "^4.17.21")]
      devDependencies rootWs `shouldBe'` mempty

      -- Check packages
      let pkgs = packages lockFile
      Map.size pkgs `shouldBe'` 1
      let lodashPkg = pkgs Map.! "lodash"
      resolution lodashPkg `shouldBe'` "lodash@4.17.21"

  describe "bun.lock with workspaces" $ do
    it' "should parse workspace bun.lock" $ do
      lockFile <- parseBunLock workspaceBunLockPath
      lockfileVersion lockFile `shouldBe'` 1

      -- Check workspaces
      let ws = workspaces lockFile
      Map.size ws `shouldBe'` 3

      -- Root workspace
      let rootWs = ws Map.! ""
      name rootWs `shouldBe'` "root-workspace"
      dependencies rootWs `shouldBe'` Map.fromList [("express", "^4.18.0")]

      -- API workspace
      let apiWs = ws Map.! "packages/api"
      name apiWs `shouldBe'` "api-package"
      dependencies apiWs `shouldBe'` Map.fromList [("axios", "^1.4.0")]

      -- Web workspace
      let webWs = ws Map.! "packages/web"
      name webWs `shouldBe'` "web-package"
      dependencies webWs `shouldBe'` Map.fromList [("react", "^18.2.0")]

      -- Check packages
      let pkgs = packages lockFile
      Map.size pkgs `shouldBe'` 3
      resolution (pkgs Map.! "express") `shouldBe'` "express@4.18.2"
      resolution (pkgs Map.! "axios") `shouldBe'` "axios@1.4.0"
      resolution (pkgs Map.! "react") `shouldBe'` "react@18.2.0"

  describe "bun.lock with dev dependencies" $ do
    it' "should parse devdeps bun.lock" $ do
      lockFile <- parseBunLock devdepsBunLockPath
      lockfileVersion lockFile `shouldBe'` 1

      -- Check workspaces
      let ws = workspaces lockFile
      Map.size ws `shouldBe'` 1
      let rootWs = ws Map.! ""
      name rootWs `shouldBe'` "devdeps-project"
      dependencies rootWs `shouldBe'` Map.fromList [("lodash", "^4.17.21")]
      devDependencies rootWs `shouldBe'` Map.fromList [("typescript", "^5.0.0"), ("jest", "^29.5.0")]

      -- Check packages
      let pkgs = packages lockFile
      Map.size pkgs `shouldBe'` 3
      resolution (pkgs Map.! "lodash") `shouldBe'` "lodash@4.17.21"
      resolution (pkgs Map.! "typescript") `shouldBe'` "typescript@5.3.3"
      resolution (pkgs Map.! "jest") `shouldBe'` "jest@29.5.0"

  describe "bun.lock with trailing commas (JSONC)" $ do
    it' "should parse bun.lock with trailing commas" $ do
      lockFile <- parseBunLock trailingCommasBunLockPath
      lockfileVersion lockFile `shouldBe'` 1

      -- Check workspaces
      let ws = workspaces lockFile
      Map.size ws `shouldBe'` 1
      let rootWs = ws Map.! ""
      name rootWs `shouldBe'` "test-project"
      Map.size (dependencies rootWs) `shouldBe'` 2
      Map.size (devDependencies rootWs) `shouldBe'` 1

      -- Check packages
      let pkgs = packages lockFile
      Map.size pkgs `shouldBe'` 3

  -- Graph building tests
  let transitiveBunLockPath = currentDir </> $(mkRelFile "test/Bun/testdata/transitive-bun.lock")

  -- Use checkGraph pattern like PnpmLockSpec
  checkGraph simpleBunLockPath simpleGraphSpec
  checkGraph workspaceBunLockPath workspaceGraphSpec
  checkGraph devdepsBunLockPath devdepsGraphSpec
  checkGraph transitiveBunLockPath transitiveGraphSpec

-- | Load a bun.lock file and run graph specs on it
checkGraph :: Path Abs File -> (Graphing Dependency -> Spec) -> Spec
checkGraph pathToFixture buildGraphSpec = do
  eitherLockFile <- runIO $ parseBunLockIO pathToFixture
  case eitherLockFile of
    Left err ->
      describe "bun.lock" $
        it "should parse lockfile" (expectationFailure err)
    Right lockFile -> buildGraphSpec (buildGraph lockFile)

-- | Parse a bun.lock file using IO (for tests)
parseBunLockIO :: Path Abs File -> IO (Either String BunLockFile)
parseBunLockIO path = do
  contents <- readFile (fromAbsFile path)
  let stripped = stripJsoncComments (Data.String.Conversion.toText contents)
      bs = Data.String.Conversion.encodeUtf8 stripped
  pure $ eitherDecodeStrict bs

-- | Convert JSONC to valid JSON (copy from BunLock.hs for testing)
-- JSONC allows: // comments and trailing commas
stripJsoncComments :: Text -> Text
stripJsoncComments input = removeTrailingCommas $ Text.unlines $ map processLine $ Text.lines input
  where
    processLine :: Text -> Text
    processLine line =
      let stripped = Text.stripStart line
       in if "//" `Text.isPrefixOf` stripped
            then ""
            else stripInlineComment line

    stripInlineComment :: Text -> Text
    stripInlineComment = go False
      where
        go :: Bool -> Text -> Text
        go _ t | Text.null t = t
        go inString t =
          case Text.uncons t of
            Nothing -> t
            Just ('"', rest)
              | not inString -> "\"" <> go True rest
              | otherwise -> "\"" <> go False rest
            Just ('\\', rest)
              | inString ->
                  case Text.uncons rest of
                    Just (c, rest') -> "\\" <> Text.singleton c <> go True rest'
                    Nothing -> "\\"
              | otherwise -> "\\" <> go inString rest
            Just ('/', rest)
              | not inString ->
                  case Text.uncons rest of
                    Just ('/', _) -> ""
                    _ -> "/" <> go inString rest
              | otherwise -> "/" <> go inString rest
            Just (c, rest) -> Text.singleton c <> go inString rest

    removeTrailingCommas :: Text -> Text
    removeTrailingCommas = go False
      where
        go :: Bool -> Text -> Text
        go _ t | Text.null t = t
        go inString t =
          case Text.uncons t of
            Nothing -> t
            Just ('"', rest)
              | not inString -> "\"" <> go True rest
              | otherwise -> "\"" <> go False rest
            Just ('\\', rest)
              | inString ->
                  case Text.uncons rest of
                    Just (c, rest') -> "\\" <> Text.singleton c <> go True rest'
                    Nothing -> "\\"
              | otherwise -> "\\" <> go inString rest
            Just (',', rest)
              | not inString ->
                  let afterWs = Text.dropWhile (`elem` [' ', '\t', '\n', '\r']) rest
                   in case Text.uncons afterWs of
                        Just ('}', _) -> go False rest
                        Just (']', _) -> go False rest
                        _ -> "," <> go False rest
              | otherwise -> "," <> go inString rest
            Just (c, rest) -> Text.singleton c <> go inString rest

simpleGraphSpec :: Graphing Dependency -> Spec
simpleGraphSpec graph = do
  describe "simple bun.lock graph" $ do
    it "marks direct dependencies from workspaces" $ do
      expectDirect [mkProdDep "lodash@4.17.21"] graph

workspaceGraphSpec :: Graphing Dependency -> Spec
workspaceGraphSpec graph = do
  describe "workspace bun.lock graph" $ do
    it "marks direct dependencies from all workspaces" $ do
      expectDirect
        [ mkProdDep "express@4.18.2"
        , mkProdDep "axios@1.4.0"
        , mkProdDep "react@18.2.0"
        ]
        graph

    it "excludes workspace packages from graph nodes" $ do
      -- Workspace packages should not appear in the graph
      -- The workspace names are: "root-workspace", "api-package", "web-package"
      -- These should be filtered out from the final graph
      let vertices = Graphing.vertexList graph
          vertexNames = map dependencyName vertices
      vertexNames `shouldBe` ["express", "axios", "react"]

devdepsGraphSpec :: Graphing Dependency -> Spec
devdepsGraphSpec graph = do
  describe "devdeps bun.lock graph" $ do
    it "labels dev dependencies with EnvDevelopment" $ do
      expectDirect
        [ mkProdDep "lodash@4.17.21"
        , mkDevDep "typescript@5.3.3"
        , mkDevDep "jest@29.5.0"
        ]
        graph

transitiveGraphSpec :: Graphing Dependency -> Spec
transitiveGraphSpec graph = do
  let hasEdge :: Dependency -> Dependency -> Expectation
      hasEdge = expectEdge graph

  describe "transitive bun.lock graph" $ do
    it "marks direct dependencies" $ do
      expectDirect
        [ mkProdDep "express@4.18.2"
        , mkDevDep "typescript@5.3.3"
        ]
        graph

    it "creates edges for transitive dependencies" $ do
      -- express -> accepts, body-parser
      hasEdge (mkProdDep "express@4.18.2") (mkProdDep "accepts@1.3.8")
      hasEdge (mkProdDep "express@4.18.2") (mkProdDep "body-parser@1.20.1")

      -- accepts -> mime-types
      hasEdge (mkProdDep "accepts@1.3.8") (mkProdDep "mime-types@2.1.35")

-- | Helper to create a production dependency
mkProdDep :: Text -> Dependency
mkProdDep nameAtVersion = mkDep nameAtVersion (Just EnvProduction)

-- | Helper to create a dev dependency
mkDevDep :: Text -> Dependency
mkDevDep nameAtVersion = mkDep nameAtVersion (Just EnvDevelopment)

-- | Helper to create a dependency from "name@version" string
mkDep :: Text -> Maybe DepEnvironment -> Dependency
mkDep nameAtVersion env = do
  let (name, version) = parseNameVersion nameAtVersion
  Dependency
    NodeJSType
    name
    (CEq <$> Just version)
    mempty
    (maybe mempty Set.singleton env)
    mempty

-- | Parse "name@version" or "@scope/name@version" into (name, version)
parseNameVersion :: Text -> (Text, Text)
parseNameVersion t
  | "@" `Text.isPrefixOf` t =
      -- Scoped package: @scope/name@version
      let withoutAt = Text.drop 1 t
          (scopeAndName, rest) = Text.breakOn "@" withoutAt
       in ("@" <> scopeAndName, Text.drop 1 rest)
  | otherwise =
      -- Regular package: name@version
      let (name, rest) = Text.breakOn "@" t
       in (name, Text.drop 1 rest)
