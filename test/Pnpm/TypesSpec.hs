module Pnpm.TypesSpec (
  spec,
) where

import Data.Bifunctor (first)
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml qualified as Yaml
import Strategy.Node.Pnpm.Types (
  DirectoryResolution (..),
  GitResolution (..),
  PackageData (..),
  PnpmCatalogs (..),
  PnpmLockFileSnapshots (..),
  PnpmLockfile (..),
  PnpmLockfileBase (..),
  PnpmLockfileV4Or5 (..),
  PnpmLockfileV678 (..),
  PnpmLockfileV9 (..),
  ProjectMap (..),
  ProjectMapDepMetadata (..),
  RegistryResolution (..),
  Resolution (..),
  TarballResolution (..),
  withoutPeerDepSuffix,
 )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

decodeLockfile :: Text -> Either String PnpmLockfile
decodeLockfile = first show . Yaml.decodeEither' . encodeUtf8

-- | Which variant a lockfile was dispatched to, or that it was rejected.
variantOf :: Either String PnpmLockfile -> String
variantOf (Left _) = "rejected"
variantOf (Right (LockfileV4Or5 _)) = "v4or5"
variantOf (Right (LockfileV678 _)) = "v678"
variantOf (Right (LockfileV9 _)) = "v9"

baseOf :: PnpmLockfile -> PnpmLockfileBase
baseOf (LockfileV4Or5 (PnpmLockfileV4Or5 b)) = b
baseOf (LockfileV678 (PnpmLockfileV678 b)) = b
baseOf (LockfileV9 v9) = lockfileBase v9

spec :: Spec
spec = do
  describe "lockfileVersion dispatch"
    $ for_
      [ ("1", "v4or5")
      , ("'4.0'", "v4or5")
      , ("5.4", "v4or5")
      , ("'5.4'", "v4or5")
      , ("'6.0'", "v678")
      , ("'7.0'", "v678")
      , ("'8.0'", "v678")
      , ("'9.0'", "v9")
      , ("'10.0'", "v9")
      , ("0", "rejected")
      , ("not-a-version", "rejected")
      ]
    $ \(ver, expected) ->
      it ("lockfileVersion " <> toString ver <> " is " <> expected) $
        variantOf (decodeLockfile ("lockfileVersion: " <> ver <> "\n")) `shouldBe` expected

  describe "lockfileVersion" $ do
    it "is rejected when missing" $
      variantOf (decodeLockfile "importers: {}\n") `shouldBe` "rejected"

    it "is kept verbatim as the raw version" $
      fmap (lockfileRawVersion . baseOf) (decodeLockfile "lockfileVersion: '6.0'\n") `shouldBe` Right "6.0"

  describe "importers" $ do
    it "treats a lockfile without importers as a single workspace at '.'" $ do
      let lockfile =
            decodeLockfile $
              Text.unlines
                [ "lockfileVersion: 5.4"
                , "dependencies:"
                , "  aws-sdk: 2.1148.0"
                , "devDependencies:"
                , "  react: 18.1.0"
                ]
      fmap (lockfileImporters . baseOf) lockfile
        `shouldBe` Right
          ( Map.singleton
              "."
              ProjectMap
                { directDependencies = Map.singleton "aws-sdk" (ProjectMapDepMetadata "2.1148.0")
                , directDevDependencies = Map.singleton "react" (ProjectMapDepMetadata "18.1.0")
                }
          )

    it "keeps explicit importers and ignores root level dependencies" $ do
      let lockfile =
            decodeLockfile $
              Text.unlines
                [ "lockfileVersion: 5.4"
                , "dependencies:"
                , "  ignored: 1.0.0"
                , "importers:"
                , "  packages/a:"
                , "    specifiers:"
                , "      commander: 9.2.0"
                , "    dependencies:"
                , "      commander: 9.2.0"
                ]
      fmap (lockfileImporters . baseOf) lockfile
        `shouldBe` Right
          ( Map.singleton
              "packages/a"
              ProjectMap
                { directDependencies = Map.singleton "commander" (ProjectMapDepMetadata "9.2.0")
                , directDevDependencies = mempty
                }
          )

    it "reads the version of a v6 importer entry" $ do
      let lockfile =
            decodeLockfile $
              Text.unlines
                [ "lockfileVersion: '6.0'"
                , "importers:"
                , "  .:"
                , "    dependencies:"
                , "      aws-sdk:"
                , "        specifier: ^2.0.0"
                , "        version: 2.1148.0"
                , "    devDependencies:"
                , "      react:"
                , "        specifier: ^18.0.0"
                , "        version: 18.1.0"
                ]
      fmap (lockfileImporters . baseOf) lockfile
        `shouldBe` Right
          ( Map.singleton
              "."
              ProjectMap
                { directDependencies = Map.singleton "aws-sdk" (ProjectMapDepMetadata "2.1148.0")
                , directDevDependencies = Map.singleton "react" (ProjectMapDepMetadata "18.1.0")
                }
          )

    it "rejects an importer dependency that is neither a string nor an object" $
      decodeLockfile
        ( Text.unlines
            [ "lockfileVersion: '6.0'"
            , "importers:"
            , "  .:"
            , "    dependencies:"
            , "      aws-sdk: [2.1148.0]"
            ]
        )
        `shouldSatisfy` either (const True) (const False)

  describe "packages" $ do
    let packageOf :: Text -> Either String PackageData
        packageOf body =
          decodeLockfile ("lockfileVersion: 5.4\npackages:\n  /pkg/1.0.0:\n" <> body)
            >>= maybe (Left "package missing") Right . Map.lookup "/pkg/1.0.0" . lockfilePackages . baseOf

    it "defaults to a production package without name, dependencies or peers" $
      packageOf "    resolution: {integrity: sha512-abc}\n"
        `shouldBe` Right
          PackageData
            { isDev = False
            , name = Nothing
            , resolution = RegistryResolve (RegistryResolution "sha512-abc")
            , dependencies = mempty
            , peerDependencies = mempty
            }

    it "reads dev, name, dependencies and peerDependencies" $
      packageOf
        ( Text.unlines
            [ "    resolution: {integrity: sha512-abc}"
            , "    name: real-name"
            , "    dev: true"
            , "    dependencies:"
            , "      buffer: 4.9.2"
            , "    peerDependencies:"
            , "      react: ^18.0.0"
            ]
        )
        `shouldBe` Right
          PackageData
            { isDev = True
            , name = Just "real-name"
            , resolution = RegistryResolve (RegistryResolution "sha512-abc")
            , dependencies = Map.singleton "buffer" "4.9.2"
            , peerDependencies = Map.singleton "react" "^18.0.0"
            }

    it "rejects a package without a resolution" $
      packageOf "    dev: false\n" `shouldSatisfy` either (const True) (const False)

    describe "resolution"
      $ for_
        [ ("{integrity: sha512-abc}", RegistryResolve (RegistryResolution "sha512-abc"))
        , ("{tarball: 'https://example.com/pkg.tgz'}", TarballResolve (TarballResolution "https://example.com/pkg.tgz"))
        , ("{repo: 'https://github.com/o/r.git', commit: abc123}", GitResolve (GitResolution "https://github.com/o/r.git" "abc123"))
        , ("{directory: ../local-pkg, type: directory}", DirectoryResolve (DirectoryResolution "../local-pkg"))
        , -- A tarball resolution also carries an integrity hash: it must stay a tarball.
          ("{tarball: 'https://example.com/pkg.tgz', integrity: sha512-abc}", TarballResolve (TarballResolution "https://example.com/pkg.tgz"))
        , -- A git resolution takes precedence over every other kind.
          ("{repo: 'https://github.com/o/r.git', commit: abc123, tarball: 'https://example.com/pkg.tgz'}", GitResolve (GitResolution "https://github.com/o/r.git" "abc123"))
        ]
      $ \(yaml, expected) ->
        it ("parses " <> toString yaml) $
          fmap resolution (packageOf ("    resolution: " <> yaml <> "\n")) `shouldBe` Right expected

    it "rejects a resolution that is none of the known kinds" $
      packageOf "    resolution: {type: unknown}\n" `shouldSatisfy` either (const True) (const False)

  describe "v9 snapshots and catalogs" $ do
    let v9Lockfile =
          decodeLockfile $
            Text.unlines
              [ "lockfileVersion: '9.0'"
              , "catalogs:"
              , "  default:"
              , "    uri-js:"
              , "      specifier: ^4.4.1"
              , "      version: 4.4.1"
              , "  react19:"
              , "    react:"
              , "      specifier: ^19.0.0"
              , "      version: 19.0.0"
              , "snapshots:"
              , "  uri-js@4.4.1:"
              , "    dependencies:"
              , "      punycode: 2.3.1"
              , "  punycode@2.3.1: {}"
              , "  react-dom@19.0.0(react@19.0.0):"
              , "    dependencies:"
              , "      react: 19.0.0"
              ]
        v9Of (Right (LockfileV9 v9)) = Right v9
        v9Of (Right _) = Left "not v9"
        v9Of (Left err) = Left err

    it "keys snapshots without the peer dependency suffix" $
      fmap (snapshots . lockfileSnapshots) (v9Of v9Lockfile)
        `shouldBe` Right
          ( HashMap.fromList
              [ ("uri-js@4.4.1", [("punycode", "2.3.1")])
              , ("punycode@2.3.1", [])
              , ("react-dom@19.0.0", [("react", "19.0.0")])
              ]
          )

    it "maps each catalog to the resolved version of its packages" $
      fmap (catalogEntries . lockfileCatalogs) (v9Of v9Lockfile)
        `shouldBe` Right
          ( Map.fromList
              [ ("default", Map.singleton "uri-js" "4.4.1")
              , ("react19", Map.singleton "react" "19.0.0")
              ]
          )

    it "defaults to no snapshots and no catalogs" $ do
      let lockfile = v9Of (decodeLockfile "lockfileVersion: '9.0'\n")
      fmap (snapshots . lockfileSnapshots) lockfile `shouldBe` Right mempty
      fmap (catalogEntries . lockfileCatalogs) lockfile `shouldBe` Right mempty

    it "does not read snapshots or catalogs for an older lockfile" $
      variantOf (decodeLockfile "lockfileVersion: '6.0'\nsnapshots:\n  a@1.0.0: {}\n") `shouldBe` "v678"

  describe "withoutPeerDepSuffix"
    $ for_
      ( [ ("1.2.0", "1.2.0")
        , ("1.2.0(babel@1.0.0)", "1.2.0")
        , ("1.2.0(babel@1.0.0)(react@18.0.0)", "1.2.0")
        , ("", "")
        ] ::
          [(Text, Text)]
      )
    $ \(input, expected) ->
      it ("strips '" <> toString input <> "' to '" <> toString expected <> "'") $
        withoutPeerDepSuffix input `shouldBe` expected
