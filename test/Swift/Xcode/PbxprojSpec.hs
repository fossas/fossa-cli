module Swift.Xcode.PbxprojSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import DepTypes (DepType (GitType, SwiftType), Dependency (..), VerConstraint (CEq))
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Swift.PackageResolved (SwiftPackageResolvedFile (..), SwiftResolvedPackage (..))
import Strategy.Swift.PackageSwift (
  SwiftPackageGitDepRequirement (..),
 )
import Strategy.Swift.Xcode.Pbxproj (
  XCRemoteSwiftPackageReference (..),
  buildGraph,
  swiftPackageReferencesOf,
 )
import Strategy.Swift.Xcode.PbxprojParser (
  AsciiValue (..),
  PbxProj (..),
  parsePbxProj,
 )
import Test.Hspec (
  Spec,
  describe,
  it,
  runIO,
  shouldBe,
 )
import Text.Megaparsec (
  parseMaybe,
 )

mockUrl :: Text
mockUrl = "mock-url"

mockId :: Text
mockId = "mock-id"

emptyPbxProj :: PbxProj
emptyPbxProj = PbxProj "" "" "" Nothing Nothing

makePbxProj :: AsciiValue -> PbxProj
makePbxProj obj = PbxProj "" "" "" Nothing (Just obj)

makePbxProjWithXCRSwiftRef :: Text -> [(Text, AsciiValue)] -> PbxProj
makePbxProjWithXCRSwiftRef url req = makePbxProj (ADict $ Map.fromList [(mockId, xcrSwiftRef)])
  where
    xcrSwiftRef =
      ADict $
        Map.fromList
          [ ("isa", AText "XCRemoteSwiftPackageReference")
          , ("repositoryURL", AText url)
          , ("requirement", ADict $ Map.fromList req)
          ]

makeXCRSwiftRef :: SwiftPackageGitDepRequirement -> XCRemoteSwiftPackageReference
makeXCRSwiftRef = XCRemoteSwiftPackageReference mockUrl

makeGitDep :: Text -> Text -> Dependency
makeGitDep name c = Dependency GitType name (CEq <$> Just c) [] [] Map.empty

makeSwiftDep :: Text -> Text -> Dependency
makeSwiftDep name c = Dependency SwiftType name (CEq <$> Just c) [] [] Map.empty

spec :: Spec
spec = do
  describe "swiftPackageReferencesOf" $ do
    it "should be empty, when there are no XCRemoteSwiftPackageReference" $ do
      let withoutIsa = makePbxProj (ADict $ Map.fromList [("A", AText "B")])
      swiftPackageReferencesOf withoutIsa `shouldBe` []

    it "should be return XCRemoteSwiftPackageReference" $ do
      -- Setup
      let branchCase =
            makePbxProjWithXCRSwiftRef
              mockUrl
              [ ("kind", AText "branch")
              , ("branch", AText "develop")
              ]
      let revisionCase =
            makePbxProjWithXCRSwiftRef
              mockUrl
              [ ("kind", AText "revision")
              , ("revision", AText "05cd")
              ]
      let exactCase =
            makePbxProjWithXCRSwiftRef
              mockUrl
              [ ("kind", AText "exactVersion")
              , ("version", AText "1.2.3")
              ]
      let upToNextMajorCase =
            makePbxProjWithXCRSwiftRef
              mockUrl
              [ ("kind", AText "upToNextMajorVersion")
              , ("minimumVersion", AText "2.0.0")
              ]
      let upToNextMinorCase =
            makePbxProjWithXCRSwiftRef
              mockUrl
              [ ("kind", AText "upToNextMinorVersion")
              , ("minimumVersion", AText "3.0.0")
              ]
      let versionRangeCase =
            makePbxProjWithXCRSwiftRef
              mockUrl
              [ ("kind", AText "versionRange")
              , ("minimumVersion", AText "4.0.0")
              , ("maximumVersion", AText "5.0.0")
              ]

      -- Assert
      swiftPackageReferencesOf branchCase `shouldBe` [makeXCRSwiftRef $ Branch "develop"]
      swiftPackageReferencesOf revisionCase `shouldBe` [makeXCRSwiftRef $ Revision "05cd"]
      swiftPackageReferencesOf exactCase `shouldBe` [makeXCRSwiftRef $ Exact "1.2.3"]
      swiftPackageReferencesOf upToNextMajorCase `shouldBe` [makeXCRSwiftRef $ UpToNextMajor "2.0.0"]
      swiftPackageReferencesOf upToNextMinorCase `shouldBe` [makeXCRSwiftRef $ UpToNextMinor "3.0.0"]
      swiftPackageReferencesOf versionRangeCase `shouldBe` [makeXCRSwiftRef $ ClosedInterval ("4.0.0", "5.0.0")]

    describe "buildGraph" $ do
      projFile <- runIO (TIO.readFile "test/Swift/Xcode/testdata/project.pbxproj")
      let projContent = fromMaybe emptyPbxProj $ parseMaybe parsePbxProj projFile

      it "should build graph of direct dependencies, when package resolved is nothing" $ do
        -- Setup
        let graph = buildGraph projContent Nothing
        let expectedDirectDeps =
              [ makeGitDep "https://github.com/apple/example-package-deckofplayingcards" "main"
              , makeGitDep "https://github.com/PopFlamingo/MyHTML.git" "2.0.0"
              , makeGitDep "https://github.com/brightdigit/Spinetail.git" "97ad8ba7a43fac299ef88f3200fccf852c778b67"
              , makeSwiftDep "https://github.com/vapor/vapor.git" ">=4.48.3 <=5.0.0"
              , makeSwiftDep "https://github.com/MartinP7r/AckGen.git" "^0.1.0"
              , makeSwiftDep "https://github.com/apple/swift-syntax" "~0.50400.0"
              ]

        -- Assert
        expectDirect expectedDirectDeps graph
        expectDeps (expectedDirectDeps) graph
        expectEdges [] graph

      it "should build graph of direct and deep dependencies, when package resolved exists" $ do
        -- Setup
        let expectedDirectDeps = [makeGitDep "dep-A" "some-rev-A"]
        let expectedDeepDeps = [makeGitDep "dep-B" "some-rev-B"]
        let mockProjContent = makePbxProjWithXCRSwiftRef "dep-A" [("kind", AText "exactVersion"), ("version", AText "1.2.5")]
        let resolvedContent =
              Just $
                SwiftPackageResolvedFile
                  1
                  [ SwiftResolvedPackage
                      "depA"
                      "dep-A"
                      Nothing
                      (Just "some-rev-A")
                      (Just "1.2.5")
                  , SwiftResolvedPackage
                      "depB"
                      "dep-B"
                      Nothing
                      (Just "some-rev-B")
                      (Just "1.2.6")
                  ]
        -- Act
        let graph = buildGraph mockProjContent resolvedContent

        -- Assert
        expectDirect expectedDirectDeps graph
        expectDeps (expectedDirectDeps ++ expectedDeepDeps) graph
        expectEdges [] graph
