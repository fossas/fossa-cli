module App.Fossa.ManualDepsTypes (
  ReferencedDependency (..),
  ManagedReferenceDependency (..),
  LinuxReferenceDependency (..),
  CustomDependency (..),
  RemoteDependency (..),
  DependencyMetadata (..),
  VendoredDependency (..),
  ManualDependencies (..),
  FoundDepsFile (..),
)
where

import Data.Aeson (
  FromJSON (parseJSON),
  Value (Null, Object),
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson.Extra (TextLike (unTextLike), forbidMembers, neText)
import Data.Aeson.Types (Object, Parser, prependFailure)
import Data.Functor.Extra ((<$$>))
import Data.String.Conversion (toString, ToText (..))
import Data.Text (Text, toLower)
import Data.Text qualified as Text
import DepTypes (DepType (..))
import Path (Abs, File, Path)
import Srclib.Types (Locator (..))
import Control.Monad (unless)

data FoundDepsFile
  = ManualYaml (Path Abs File)
  | ManualJSON (Path Abs File)

-- TODO: Change these to Maybe NonEmpty
data ManualDependencies = ManualDependencies
  { referencedDependencies :: [ReferencedDependency]
  , customDependencies :: [CustomDependency]
  , vendoredDependencies :: [VendoredDependency]
  , remoteDependencies :: [RemoteDependency]
  , locatorDependencies :: [Locator]
  }
  deriving (Eq, Ord, Show)

instance FromJSON ManualDependencies where
  parseJSON (Object obj) =
    ManualDependencies
      <$ (obj .:? "version" >>= isMissingOr1)
      <*> (obj .:? "referenced-dependencies" .!= [])
      <*> (obj .:? "custom-dependencies" .!= [])
      <*> (obj .:? "vendored-dependencies" .!= [])
      <*> (obj .:? "remote-dependencies" .!= [])
      <*> (obj .:? "locator-dependencies" .!= [])
    where
      isMissingOr1 :: Maybe Int -> Parser ()
      isMissingOr1 (Just x) | x /= 1 = fail $ "Invalid fossa-deps version: " <> show x
      isMissingOr1 _ = pure ()
  parseJSON (Null) = pure $ ManualDependencies mempty mempty mempty mempty mempty
  parseJSON other = fail $ "Expected object or Null for ManualDependencies, but got: " <> show other

data ReferencedDependency
  = Managed ManagedReferenceDependency
  | LinuxApkDebDep LinuxReferenceDependency
  | LinuxRpmDep LinuxReferenceDependency (Maybe Text)
  deriving (Eq, Ord, Show)

data ManagedReferenceDependency = ManagedReferenceDependency
  { locDepName :: Text
  , locDepType :: DepType
  , locDepVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data LinuxReferenceDependency = LinuxReferenceDependency
  { locLinuxDepName :: Text
  , locLinuxDepType :: DepType
  , locLinuxDepVersion :: Maybe Text
  , locLinuxDepArch :: Text
  , locLinuxDepOS :: Text
  , locLinuxDepOSVersion :: Text
  }
  deriving (Eq, Ord, Show)


instance FromJSON ReferencedDependency where
  parseJSON = withObject "ReferencedDependency" $ \obj -> do
    depType <- parseDepType obj
    case depType of
      LinuxRPM -> parseRpmDependency obj depType
      LinuxAPK -> parseApkOrDebDependency obj depType
      LinuxDEB -> parseApkOrDebDependency obj depType
      _ -> parseManagedDependency obj depType
    where
      parseDepType :: Object -> Parser DepType
      parseDepType obj = obj .: "type" >>= depTypeParser

      parseManagedDependency :: Object -> DepType -> Parser ReferencedDependency
      parseManagedDependency obj depType =
        Managed
          <$> ( ManagedReferenceDependency
                  <$> (obj `neText` "name")
                  <*> pure depType
                  <*> (unTextLike <$$> obj .:? "version")
                  <* forbidNonRefDepFields obj
                  <* forbidLinuxFields depType obj
                  <* forbidEpoch depType obj
              )

      parseApkOrDebDependency :: Object -> DepType -> Parser ReferencedDependency
      parseApkOrDebDependency obj depType =
        LinuxApkDebDep
          <$> parseLinuxDependency obj depType
          <* forbidNonRefDepFields obj
          <* forbidEpoch depType obj

      parseRpmDependency :: Object -> DepType -> Parser ReferencedDependency
      parseRpmDependency obj depType =
        LinuxRpmDep
          <$> parseLinuxDependency obj depType
          <*> (unTextLike <$$> obj .:? "epoch")
          <* forbidNonRefDepFields obj

      parseLinuxDependency :: Object -> DepType -> Parser LinuxReferenceDependency
      parseLinuxDependency obj depType =
        LinuxReferenceDependency
          <$> (obj `neText` "name")
          <*> pure depType
          <*> (unTextLike <$$> obj .:? "version")
          <*> parseArch obj
          <*> parseOS obj
          <*> parseOSVersion obj

      parseArch :: Object -> Parser Text
      parseArch obj = requiredFieldMsg "arch" $ obj .: "arch"

      parseOSVersion :: Object -> Parser Text
      parseOSVersion obj = requiredFieldMsg "osVersion" (unTextLike <$> obj .: "osVersion")

      parseOS :: Object -> Parser Text
      parseOS obj = do
        os <- requiredFieldMsg "os" $ obj .: "os"
        unless (toLower os `elem` supportedOSs)
          $ fail
            . toString
          $ "Provided os: "
            <> (toLower os)
            <> " is not supported! Please provide oneOf: "
            <> Text.intercalate ", " supportedOSs
        pure os

      requiredFieldMsg :: String -> Parser a -> Parser a
      requiredFieldMsg field =
        prependFailure $ field <> " is required field for reference dependency (of dependency type: apk, deb, rpm-generic): "

      forbidLinuxFields :: DepType -> Object -> Parser ()
      forbidLinuxFields depType =
        forbidMembers
          ("referenced dependencies (of dependency type: " <> depTypeToText depType <> ")")
          [ "os"
          , "osVersion"
          , "arch"
          ]

      forbidEpoch :: DepType -> Object -> Parser ()
      forbidEpoch depType =
        forbidMembers
          ("referenced dependencies (of dependency type: " <> depTypeToText depType <> ")")
          ["epoch"]

      forbidNonRefDepFields :: Object -> Parser ()
      forbidNonRefDepFields =
        forbidMembers
          "referenced dependencies"
          [ "license"
          , "description"
          , "url"
          , "path"
          ]

      depTypeParser :: Text -> Parser DepType
      depTypeParser text = case depTypeFromText text of
        Just t -> pure t
        Nothing -> fail $ "dep type: " <> toString text <> " not supported"

      -- Parse supported dependency types into their respective type or return Nothing.
      depTypeFromText :: Text -> Maybe DepType
      depTypeFromText text = case text of
        "bower" -> Just BowerType
        "cargo" -> Just CargoType
        "carthage" -> Just CarthageType
        "composer" -> Just ComposerType
        "cpan" -> Just CpanType
        "cran" -> Just CranType
        "gem" -> Just GemType
        "git" -> Just GitType
        "go" -> Just GoType
        "hackage" -> Just HackageType
        "hex" -> Just HexType
        "maven" -> Just MavenType
        "npm" -> Just NodeJSType
        "nuget" -> Just NuGetType
        "pypi" -> Just PipType
        "cocoapods" -> Just PodType
        "url" -> Just URLType
        "swift" -> Just SwiftType
        "apk" -> Just LinuxAPK
        "deb" -> Just LinuxDEB
        "rpm-generic" -> Just LinuxRPM
        _ -> Nothing -- unsupported dep, need to respond with an error and skip this dependency

      depTypeToText :: DepType -> Text
      depTypeToText depType = case depType of
        BowerType -> "bower"
        CargoType -> "cargo"
        CarthageType -> "carthage"
        ComposerType -> "composer"
        CpanType -> "cpan"
        CranType -> "cran"
        GemType -> "gem"
        GitType -> "git"
        GoType -> "go"
        HackageType -> "hackage"
        HexType -> "hex"
        MavenType -> "maven"
        NodeJSType -> "npm"
        NuGetType -> "nuget"
        PipType -> "pypi"
        PodType -> "cocoapods"
        URLType -> "url"
        SwiftType -> "swift"
        LinuxAPK -> "apk"
        LinuxDEB -> "deb"
        LinuxRPM -> "rpm"
        other -> toText . show $ other

      -- | Distro OS supported by FOSSA.
      -- If you update this, please make sure to update /docs/references/files/fossa-deps.schema.json
      supportedOSs :: [Text]
      supportedOSs =
        [ "alpine"
        , "centos"
        , "debian"
        , "redhat"
        , "ubuntu"
        , "oraclelinux"
        , "busybox"
        , "sles"
        , "fedora"
        ]

data CustomDependency = CustomDependency
  { customName :: Text
  , customVersion :: Text
  , customLicense :: Text
  , customMetadata :: Maybe DependencyMetadata
  }
  deriving (Eq, Ord, Show)

instance FromJSON CustomDependency where
  parseJSON = withObject "CustomDependency" $ \obj ->
    CustomDependency
      <$> (obj `neText` "name")
      <*> (unTextLike <$> obj `neText` "version")
      <*> (obj `neText` "license")
      <*> obj .:? "metadata"
      <* forbidMembers "custom dependencies" ["type", "path", "url"] obj

data RemoteDependency = RemoteDependency
  { remoteName :: Text
  , remoteVersion :: Text
  , remoteUrl :: Text
  , remoteMetadata :: Maybe DependencyMetadata
  }
  deriving (Eq, Ord, Show)

instance FromJSON RemoteDependency where
  parseJSON = withObject "RemoteDependency" $ \obj -> do
    RemoteDependency
      <$> (obj `neText` "name")
      <*> (unTextLike <$> obj `neText` "version")
      <*> (obj `neText` "url")
      <*> obj .:? "metadata"
      <* forbidMembers "remote dependencies" ["license", "path", "type"] obj

data DependencyMetadata = DependencyMetadata
  { depDescription :: Maybe Text
  , depHomepage :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON DependencyMetadata where
  parseJSON = withObject "metadata" $ \obj ->
    DependencyMetadata
      <$> obj .:? "description"
      <*> obj .:? "homepage"
      <* forbidMembers "metadata" ["url"] obj

data VendoredDependency = VendoredDependency
  { vendoredName :: Text
  , vendoredPath :: Text
  , vendoredVersion :: Maybe Text
  , vendoredMetadata :: Maybe DependencyMetadata
  }
  deriving (Eq, Ord, Show)

instance FromJSON VendoredDependency where
  parseJSON = withObject "VendoredDependency" $ \obj -> do
    vendorDep <-
      VendoredDependency
        <$> (obj `neText` "name")
        <*> (obj `neText` "path")
        <*> (unTextLike <$$> obj .:? "version")
        <*> (obj .:? "metadata")
        <* forbidMembers "vendored dependencies" ["type", "license", "url", "description"] obj

    case vendoredVersion vendorDep of
      Nothing -> pure vendorDep
      Just version' -> do
        let fcInVersion = filter (`Text.isInfixOf` version') forbiddenChars
        if null fcInVersion
          then pure vendorDep
          else
            fail $
              "field 'version' conatins forbidden character(s): "
                <> show fcInVersion
                <> "! Do not use anyof: "
                <> show forbiddenChars
    where
      -- If following charcters are allowed in version
      -- We end up with 403 error from S3. This is likely
      -- due to encoding/escaping issue. Refer to backlog.
      forbiddenChars :: [Text]
      forbiddenChars = ["?", "#"]
