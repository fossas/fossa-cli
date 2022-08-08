module Container.TarballSpec (
  spec,
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry (Entry (entryTarPath), fromTarPathToPosixPath)
import Codec.Archive.Tar.Index (TarEntryOffset)
import Container.Docker.ImageJson (ImageJson (ImageJson), ImageJsonRootFs (ImageJsonRootFs))
import Container.Tarball (
  TarEntries (TarEntries),
  mkEntries,
  mkImage,
  removeWhiteOut,
 )
import Container.Types (
  ContainerFSChangeSet (InsertOrUpdate, Whiteout),
  ContainerImageRaw (ContainerImageRaw),
  ContainerLayer (layerChangeSets),
 )
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as ByteStringLazy
import Data.Foldable (toList)
import Data.List.NonEmpty qualified as NLE
import Test.Hspec (
  Spec,
  describe,
  expectationFailure,
  it,
  runIO,
  shouldBe,
 )

removeWhiteOutSpec :: Spec
removeWhiteOutSpec =
  describe "removeWhiteOut" $ do
    it "should return path without whiteout marker, when a directory has whiteout marker" $ do
      removeWhiteOut ".wh.data.log" `shouldBe` "data.log"
      removeWhiteOut "etc/.wh.data.log" `shouldBe` "etc/data.log"
      removeWhiteOut "etc/nested/.wh.data.log" `shouldBe` "etc/nested/data.log"

    it "should return Nothing, when a path does not have a whiteout marker" $ do
      removeWhiteOut "etc/archive/" `shouldBe` "etc/archive/"
      removeWhiteOut "etc/nested/archive/" `shouldBe` "etc/nested/archive/"
      removeWhiteOut "data.log" `shouldBe` "data.log"
      removeWhiteOut "etc/data.log" `shouldBe` "etc/data.log"
      removeWhiteOut "etc/nested/data.log" `shouldBe` "etc/nested/data.log"

exampleImg :: FilePath
exampleImg = "test/Container/testdata/changeset_example.tar"

exampleImgLayers :: NLE.NonEmpty FilePath
exampleImgLayers =
  NLE.fromList
    [ -- Ignore base layer since it is provided base image;
      -- "e41b68215b5165f6f14e274c208885c0f4ee3766d9404b68ed679e88eada0021/layer.tar",
      "c93a3bb6976b4b84aa147a657a6ee319046d1556f2e0a97f59d9fb71c3f97dee/layer.tar"
    , "151cd8679f39b8f3a595ad0f1cdd022732c0e0d893995f23e9a8b32b23038e7b/layer.tar"
    , "1e019fb5d397ea20b463b0419da6526daf9725c257f567f641f07278937f16a6/layer.tar"
    , "bc59e5f3a95799b3e19d4a7c0472b2f4bfacadb94adbd4463c8beee27f5c3e7e/layer.tar"
    , "69fa65e4ea24eaba63bc404184a1c673420cfa799b1ae6fd18fe7f989631c46e/layer.tar"
    , "efef3a67f85b6bf3b9584832aeb824c7c5533ee4ac3443d2e3523130407e3ebe/layer.tar"
    , "e8562e2ca2eec04dec1b798010628430edeb86b0ebbd6be5a637e2b472414531/layer.tar"
    ]

mkEntriesSpec :: Spec
mkEntriesSpec = do
  tarFile <- runIO $ Tar.read <$> ByteStringLazy.readFile exampleImg

  describe "mkEntries" $
    it "should include entries of files with offsets" $
      case mkEntries tarFile of
        Left err -> expectationFailure (show err)
        Right entries -> do
          tarEntriesToPathsOffset entries
            `shouldBe` [ ("151cd8679f39b8f3a595ad0f1cdd022732c0e0d893995f23e9a8b32b23038e7b/", 0)
                       , ("151cd8679f39b8f3a595ad0f1cdd022732c0e0d893995f23e9a8b32b23038e7b/VERSION", 1)
                       , ("151cd8679f39b8f3a595ad0f1cdd022732c0e0d893995f23e9a8b32b23038e7b/json", 3)
                       , ("151cd8679f39b8f3a595ad0f1cdd022732c0e0d893995f23e9a8b32b23038e7b/layer.tar", 5)
                       , ("1e019fb5d397ea20b463b0419da6526daf9725c257f567f641f07278937f16a6/", 22)
                       , ("1e019fb5d397ea20b463b0419da6526daf9725c257f567f641f07278937f16a6/VERSION", 23)
                       , ("1e019fb5d397ea20b463b0419da6526daf9725c257f567f641f07278937f16a6/json", 25)
                       , ("1e019fb5d397ea20b463b0419da6526daf9725c257f567f641f07278937f16a6/layer.tar", 27)
                       , ("69fa65e4ea24eaba63bc404184a1c673420cfa799b1ae6fd18fe7f989631c46e/", 36)
                       , ("69fa65e4ea24eaba63bc404184a1c673420cfa799b1ae6fd18fe7f989631c46e/VERSION", 37)
                       , ("69fa65e4ea24eaba63bc404184a1c673420cfa799b1ae6fd18fe7f989631c46e/json", 39)
                       , ("69fa65e4ea24eaba63bc404184a1c673420cfa799b1ae6fd18fe7f989631c46e/layer.tar", 41)
                       , ("7b27f90216d827d7b4ad2b679c276201b250d10408481b81d0c0a42d37e177e9.json", 49)
                       , ("bc59e5f3a95799b3e19d4a7c0472b2f4bfacadb94adbd4463c8beee27f5c3e7e/", 56)
                       , ("bc59e5f3a95799b3e19d4a7c0472b2f4bfacadb94adbd4463c8beee27f5c3e7e/VERSION", 57)
                       , ("bc59e5f3a95799b3e19d4a7c0472b2f4bfacadb94adbd4463c8beee27f5c3e7e/json", 59)
                       , ("bc59e5f3a95799b3e19d4a7c0472b2f4bfacadb94adbd4463c8beee27f5c3e7e/layer.tar", 61)
                       , ("c93a3bb6976b4b84aa147a657a6ee319046d1556f2e0a97f59d9fb71c3f97dee/", 69)
                       , ("c93a3bb6976b4b84aa147a657a6ee319046d1556f2e0a97f59d9fb71c3f97dee/VERSION", 70)
                       , ("c93a3bb6976b4b84aa147a657a6ee319046d1556f2e0a97f59d9fb71c3f97dee/json", 72)
                       , ("c93a3bb6976b4b84aa147a657a6ee319046d1556f2e0a97f59d9fb71c3f97dee/layer.tar", 74)
                       , ("e41b68215b5165f6f14e274c208885c0f4ee3766d9404b68ed679e88eada0021/", 84)
                       , ("e41b68215b5165f6f14e274c208885c0f4ee3766d9404b68ed679e88eada0021/VERSION", 85)
                       , ("e41b68215b5165f6f14e274c208885c0f4ee3766d9404b68ed679e88eada0021/json", 87)
                       , ("e41b68215b5165f6f14e274c208885c0f4ee3766d9404b68ed679e88eada0021/layer.tar", 89)
                       , ("e8562e2ca2eec04dec1b798010628430edeb86b0ebbd6be5a637e2b472414531/", 11565)
                       , ("e8562e2ca2eec04dec1b798010628430edeb86b0ebbd6be5a637e2b472414531/VERSION", 11566)
                       , ("e8562e2ca2eec04dec1b798010628430edeb86b0ebbd6be5a637e2b472414531/json", 11568)
                       , ("e8562e2ca2eec04dec1b798010628430edeb86b0ebbd6be5a637e2b472414531/layer.tar", 11571)
                       , ("efef3a67f85b6bf3b9584832aeb824c7c5533ee4ac3443d2e3523130407e3ebe/", 11575)
                       , ("efef3a67f85b6bf3b9584832aeb824c7c5533ee4ac3443d2e3523130407e3ebe/VERSION", 11576)
                       , ("efef3a67f85b6bf3b9584832aeb824c7c5533ee4ac3443d2e3523130407e3ebe/json", 11578)
                       , ("efef3a67f85b6bf3b9584832aeb824c7c5533ee4ac3443d2e3523130407e3ebe/layer.tar", 11580)
                       , ("manifest.json", 11585)
                       , ("repositories", 11588)
                       ]

mockImageJson :: ImageJson
mockImageJson =
  ImageJson . ImageJsonRootFs $
    NLE.fromList
      [ "sha256:b541d28bf3b491aeb424c61353c8c92476ecc2cd603a6c09ee5c2708f1a4b258"
      , "sha256:690b9450535c0e7db4f6a9f41a15e3260abfec49d0430f4a853185d15af89f20"
      , "sha256:3859b16f69447c6a8e59659d7d6e629dba1c5a87dba6b9374fad0e1d98ede98d"
      , "sha256:80169933fb42b41773031cab68d4688c96dd69e094507a8fe8b74e253f047648"
      , "sha256:1ed3dd0e0a49ff255a219191ea3bfffa1e3d5ff99647b732923237e87d548cce"
      , "sha256:f362a8928301b2ba83eb44d9e729c8f9cdabce2049b14f164637ae6fffbb8800"
      , "sha256:33552eb17ad8ae902c15a8037e0fe69c85bc8c1af6c3bbc7258f41feafb2e082"
      , "sha256:3892250d356b09c234bb80bd28a6a2aad35e0049be30391d5bff03c2674be3d2"
      ]

mkImageSpec :: Spec
mkImageSpec = do
  tarFile <- runIO $ Tar.read <$> ByteStringLazy.readFile exampleImg

  describe "mkImage" $
    it "should build image with layers with all change sets" $
      case mkEntries tarFile of
        Left err -> expectationFailure (show err)
        Right entries -> do
          case (mkImage mempty mockImageJson entries exampleImgLayers) of
            Left errs -> expectationFailure (show errs)
            Right img ->
              toChangeSets img
                `shouldBe` [
                             -- Layer 1 - Adds a file and directory

                             [ InsertOrUpdate "health.txt" 76
                             , InsertOrUpdate "status.txt" 80
                             ]
                           , -- Layer 2 - Adds a nested file and a nested directory

                             [ InsertOrUpdate "logs-archive/feb/2.txt" 10
                             , InsertOrUpdate "logs-archive/jan/1.txt" 14
                             , InsertOrUpdate "logs-archive/last.txt" 16
                             ]
                           , -- Layer 3 - Removes a file and directories

                             [ Whiteout "logs-archive/jan/1.txt"
                             , Whiteout "logs-archive/march"
                             , Whiteout "status.txt"
                             ]
                           , -- Layer 4 - Adds an absolute symbolic link and relative symbolic link

                             [ InsertOrUpdate "last" 63
                             , InsertOrUpdate "logs-archive/feb/last_health" 66
                             ]
                           , -- Layer 5 - Removes symbolic link

                             [ Whiteout "last"
                             , Whiteout "logs-archive/feb/last_health"
                             ]
                           , -- # Layer 6 - Removes a nested directory

                             [ Whiteout "logs-archive"
                             ]
                           , -- # Layer 7 - Empty layer
                             []
                           ]

spec :: Spec
spec = do
  removeWhiteOutSpec
  mkEntriesSpec
  mkImageSpec

tarEntriesToPathsOffset :: TarEntries -> [(FilePath, TarEntryOffset)]
tarEntriesToPathsOffset (TarEntries entries _) = map (first $ fromTarPathToPosixPath . entryTarPath) $ toList entries

toChangeSets :: ContainerImageRaw -> [[ContainerFSChangeSet]]
toChangeSets (ContainerImageRaw layers _) = map (toList . layerChangeSets) (toList layers)
