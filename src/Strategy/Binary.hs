module Strategy.Binary
  ( discover,
    getDeps,
  )
where

import App.Fossa.Analyze.Types (AnalyzeProject (..))
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withExecSnooperBinary)
import Control.Effect.Diagnostics
    ( Diagnostics, Has, context, fatalText, recover )
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader)
import Control.Monad (filterM)
import Data.Aeson
  ( FromJSON (parseJSON),
    eitherDecode,
    withObject,
    (.:),
  )
import Data.Aeson.Types
  ( ToJSON,
  )
import Data.ByteString (ByteString, isPrefixOf, pack)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (fromStrict)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk
  ( WalkStep (WalkContinue),
    walkWithFilters',
  )
import Effect.Exec
  ( AllowErr (Never),
    Command (..),
    Exec, execStdinBsThrow,
  )
import Effect.ReadFS (ReadFS, readContentsBS, readContentsBSLimit)
import GHC.Generics (Generic)
import Graphing (Graphing, directs, direct)
import Path (Abs, Dir, File, Path)
import Types
  ( DepType (..),
    Dependency (..),
    DependencyResults (..),
    DiscoveredProject (..),
    DiscoveredProjectType (..),
    GraphBreadth (Partial),
    VerConstraint (CEq),
  )
import Control.Applicative ((<|>))
import App.Fossa.VSI.Fingerprint (fingerprintRawBs, Fingerprint, Raw)
import qualified Path.IO as PIO
import Control.Carrier.Lift (sendIO)

data ParsableBinaryProject = ParsableBinaryProject
  { binaryDir :: Path Abs Dir,
    binaryPath :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ParsableBinaryProject

instance FromJSON ParsableBinaryProject

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject ParsableBinaryProject]
discover = simpleDiscover findProjects mkProject ParsableExecutableProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [ParsableBinaryProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  binFiles <- filterM isValidExecFile files
  case binFiles of
    [] -> pure ([], WalkContinue)
    xs -> do
      projects <- traverse (\file -> pure $ ParsableBinaryProject {binaryDir = dir, binaryPath = file}) xs
      pure (projects, WalkContinue)

isValidExecFile :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m Bool
isValidExecFile file = do
  header <- recover $ readContentsBSLimit file 16
  case header of
    Nothing -> pure False
    Just header' -> do
      let headerExceptFirstByte = BS.drop 1 header'
      let isElf = elfHeader `isPrefixOf` header'
      let isPe = peHeader `isPrefixOf` header'
      let isMachO = machoHeader `isPrefixOf` header' || machoHeaderLittleEndian `isPrefixOf` headerExceptFirstByte || machoUniversalHeader `isPrefixOf` header'
      case (isElf, isPe, isMachO) of
        (True, _, _) -> pure True
        (_, True, _) -> pure True
        (_, _, True) -> pure True
        _ -> pure False

elfHeader :: ByteString
elfHeader = pack [0x7F, 0x45, 0x4C, 0x46]

peHeader :: ByteString
peHeader = "MZ"

machoHeader :: ByteString
machoHeader = pack [0xFE, 0xED, 0xFA]

machoHeaderLittleEndian :: ByteString
machoHeaderLittleEndian = pack [0xFA, 0xED, 0xFE]

machoUniversalHeader :: ByteString
machoUniversalHeader = pack [0xCA, 0xFE, 0xBA, 0xBE]

mkProject :: ParsableBinaryProject -> DiscoveredProject ParsableBinaryProject
mkProject project =
  DiscoveredProject
    { projectType = ParsableExecutableProjectType,
      projectBuildTargets = mempty,
      projectPath = binaryDir project,
      projectData = project
    }

analyze ::
  (Has Exec sig m, Has Diagnostics sig m, Has (Lift IO) sig m, Has ReadFS sig m) =>
  ParsableBinaryProject ->
  m (Graphing Dependency)
analyze (ParsableBinaryProject _ execFile) = withExecSnooperBinary $ \bin -> do
  bs <- readContentsBS execFile
  currDir <- sendIO PIO.getCurrentDir
  stdout <- recover $ execStdinBsThrow (execSnooperCommand bin) currDir (fromStrict bs)
  execInfo <- case stdout of
    Just stdout' -> do
      case eitherDecode stdout' of
        Right (a :: ExecInfo) -> pure a
        Left err -> fatalText $ "could not parse stdout from cmd: " <> toText err
    Nothing -> do
      fp <- fingerprintRawBs bs
      pure (ExecUnknown fp)

  case execInfo of
    (ExecGoBuildInfo goBuildInfo) -> pure $ analyzeGo goBuildInfo
    (ExecRustBuildInfo rustBuildInfo) -> pure $ analyzeRust rustBuildInfo
    (ExecUnknown fp) -> pure $ analyzeUnknown (toText execFile) fp

instance AnalyzeProject ParsableBinaryProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDeps

execSnooperCommand :: BinaryPaths -> Command
execSnooperCommand bin =
  Command
    { cmdName = toText $ toPath bin,
      cmdArgs = [],
      cmdAllowErr = Never
    }

getDeps :: (Has Exec sig m, Has Diagnostics sig m, Has ReadFS sig m, Has (Lift IO) sig m) => ParsableBinaryProject -> m DependencyResults
getDeps project = do
  graph <- context "Parsable Binary" . context "Dynamic analysis" . analyze $ project
  pure $
    DependencyResults
      { dependencyGraph = graph,
        dependencyGraphBreadth = Partial,
        dependencyManifestFiles = [binaryPath project]
      }

data ExecInfo =
  ExecGoBuildInfo GolangBuildInfo
  | ExecRustBuildInfo RustBuildInfo
  | ExecUnknown (Fingerprint Raw)
  deriving (Eq, Show, Generic)

instance FromJSON ExecInfo where
  parseJSON = withObject "ExecInfo" $ \obj ->
    (ExecGoBuildInfo <$> obj .: "GolangExecutable")
    <|> (ExecRustBuildInfo <$> obj .: "RustExecutable")

data GolangBuildInfo = GolangBuildInfo
  { golangBuildInfoGoVersion :: Text,
    golangBuildInfoPath :: Text,
    golangBuildInfoMain :: GolangBuildInfoDep,
    golangBuildInfoDeps :: [GolangBuildInfoDep]
  }
  deriving (Eq, Ord, Show, Generic)

data GolangBuildInfoDep = GolangBuildInfoDep
  { golangBuildInfoDepPath :: Text,
    golangBuildInfoDepVersion :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON GolangBuildInfo where
  parseJSON = withObject "GolangBuildInfo" $ \obj ->
    GolangBuildInfo
      <$> obj .: "GoVersion"
      <*> obj .: "Path"
      <*> obj .: "Main"
      <*> obj .: "Deps"

instance FromJSON GolangBuildInfoDep where
  parseJSON = withObject "GolangBuildInfo" $ \obj ->
    GolangBuildInfoDep
      <$> obj .: "Path"
      <*> obj .: "Version"

newtype RustBuildInfo
  = RustBuildInfo { rustBuildInfoPackages :: [RustBuildInfoPackage]}
  deriving (Eq, Ord, Show, Generic)

data RustBuildInfoPackage = RustBuildInfoPackage {
  rustBuildInfoPackageName :: Text,
  rustBuildInfoPackageVersion :: Text,
  rustBuildInfoPackageSource :: Text
} deriving (Eq, Ord, Show, Generic)

instance FromJSON RustBuildInfo where
  parseJSON = withObject "RustBuildInfo" $ \obj ->
    RustBuildInfo
      <$> obj .: "packages"

instance FromJSON RustBuildInfoPackage where
  parseJSON = withObject "RustBuildInfoPackage" $ \obj ->
    RustBuildInfoPackage
      <$> obj .: "name"
      <*> obj .: "version"
      <*> obj .: "source"

analyzeGo :: GolangBuildInfo -> Graphing Dependency
analyzeGo buildInfo = directs (map goInfoDepToDep $ golangBuildInfoDeps buildInfo)

goInfoDepToDep :: GolangBuildInfoDep -> Dependency
goInfoDepToDep infoDep =
  Dependency
    { dependencyType = GoType,
      dependencyName = golangBuildInfoDepPath infoDep,
      dependencyVersion = Just . CEq . golangBuildInfoDepVersion $ infoDep,
      dependencyLocations = mempty,
      dependencyEnvironments = mempty,
      dependencyTags = mempty
    }

analyzeRust :: RustBuildInfo -> Graphing Dependency
analyzeRust buildInfo = directs (map rustBuildInfoPackageToDep $ rustBuildInfoPackages buildInfo)

rustBuildInfoPackageToDep :: RustBuildInfoPackage -> Dependency
rustBuildInfoPackageToDep pkg =
  Dependency
    { dependencyType = CargoType,
      dependencyName = rustBuildInfoPackageName pkg,
      dependencyVersion = Just . CEq . rustBuildInfoPackageVersion $ pkg,
      dependencyLocations = mempty,
      dependencyEnvironments = mempty,
      dependencyTags = mempty
    }

analyzeUnknown :: Text -> Fingerprint Raw -> Graphing Dependency
analyzeUnknown name fp = direct $ Dependency
    { dependencyType = UnknownBinaryType,
      dependencyName = name,
      dependencyVersion = Just . CEq $ toText fp,
      dependencyLocations = mempty,
      dependencyEnvironments = mempty,
      dependencyTags = mempty
    }