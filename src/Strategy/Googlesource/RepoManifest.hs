{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}

module Strategy.Googlesource.RepoManifest
  ( discover
  , buildGraph
  , analyze
  , validateProject
  , validateProjects
  , nestedValidatedProjects
  , ManifestGitConfigError
  , RepoManifest(..)
  , ManifestRemote(..)
  , ManifestDefault(..)
  , ManifestProject(..)
  , ValidatedProject(..)

  , mkProjectClosure
  ) where

import Prologue

import Control.Carrier.Error.Either
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing, unfold)
import Parse.XML
import Types
import Text.URI
import Text.GitConfig.Parser (Section(..), parseConfig)
import qualified Data.HashMap.Strict as HM
import Text.Megaparsec (errorBundlePretty)
import Effect.ErrorUtils (tagError)

-- We're looking for a file called "manifest.xml" in a directory called ".repo"
discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files ->
  case find (\f -> "manifest.xml" == fileName f) files of
    Nothing -> pure WalkContinue
    Just file ->
      if dirname (parent file) == $(mkRelDir ".repo") then do
        runSimpleStrategy "googlesource-repomanifest" GooglesourceGroup $ analyze file
        pure WalkSkipAll
      else pure WalkContinue

analyze :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m, MonadFail m, Effect sig) => Path Rel File -> m ProjectClosureBody
analyze file = do
  validatedProjects <- runError @ManifestGitConfigError $ nestedValidatedProjects (parent file) file
  case validatedProjects of
    Left err -> fail $ show err
    Right projects -> pure $ mkProjectClosure file projects

nestedValidatedProjects :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m, Has (Error ManifestGitConfigError) sig m, Effect sig, MonadFail m) => Path Rel Dir -> Path Rel File -> m [ValidatedProject]
nestedValidatedProjects rootDir file = do
  manifest <- readContentsXML @RepoManifest file
  manifestWithFixedRemotes <- fixRelativeRemotes manifest rootDir
  validatedIncludedProjects <- validatedProjectsFromIncludes manifestWithFixedRemotes (parent file) rootDir
  let validatedDirectProjects = validateProjects manifestWithFixedRemotes
  case validatedDirectProjects of
    Nothing -> fail "Error creating validated projects"
    Just ps -> pure $ ps ++ validatedIncludedProjects

fixRelativeRemotes :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m, Has (Error ManifestGitConfigError) sig m) => RepoManifest -> Path Rel Dir -> m RepoManifest
fixRelativeRemotes manifest rootDir = do
  let remotes = manifestRemotes manifest
  fixedRemotes <- traverse (fixRemote rootDir) remotes
  pure $ manifest {manifestRemotes = fixedRemotes}

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

fixRemote :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m, Has (Error ManifestGitConfigError) sig m) => Path Rel Dir -> ManifestRemote -> m ManifestRemote
fixRemote rootDir remote = do
  let configFile = rootDir </> $(mkRelFile "manifests.git/config")

  exists <- doesFileExist configFile
  unless exists (throwError $ MissingGitConfig $ T.pack $ show configFile)

  contents <- readContentsText configFile
  config <- tagError (GitConfigParse . T.pack . errorBundlePretty) (parseConfig contents)

  let fixedUri :: Either Text URI
      fixedUri = do
        (Section _ properties) <- maybeToEither "No section found" $ find isOrigin config
        remoteUrl <- maybeToEither "url lookup failed in git remote" (HM.lookup "url" properties)
        rUrl <- maybeToEither "mkURI failed on rUrl" $ mkURI remoteUrl
        fUrl <- maybeToEither "mkURI failed on remote fetch URL" $ mkURI $ remoteFetch remote
        maybeToEither ("relativeTo failed for URLs remoteUrl = " <> remoteUrl <> " and remoteFetch remote = " <> remoteFetch remote)
                      (fUrl `relativeTo` rUrl)

  url <- tagError InvalidRemote fixedUri
  pure $ remote { remoteFetch = render url }

  where
  isOrigin :: Section -> Bool
  isOrigin (Section ["remote", "origin"] _) = True
  isOrigin _ = False

-- If a manifest has an include tag, the included manifest will be found in "manifests/<name attribute>" relative
-- to the original manifest file.
-- A minimal manifest with an include tag will look like this:
-- <manifest>
--   <include name="default.xml" />
-- </manifest>
-- If you see that, you need to look for `manifests/default.xml`, where the manifests directory will
-- be a sibling to the original manifest you were parsing.
validatedProjectsFromIncludes :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m, Has (Error ManifestGitConfigError) sig m, Effect sig, MonadFail m) => RepoManifest -> Path Rel Dir -> Path Rel Dir -> m [ValidatedProject]
validatedProjectsFromIncludes manifest parentDir rootDir = do
    let manifestIncludeFiles :: [Text]
        manifestIncludeFiles = map includeName $ manifestIncludes manifest
        pathRelativeToManifestDir :: Text -> Maybe (Path Rel File)
        pathRelativeToManifestDir file = (parentDir </>) <$> parseRelFile ("manifests/" ++ T.unpack file)
        manifestFiles :: Maybe [Path Rel File]
        manifestFiles = traverse pathRelativeToManifestDir manifestIncludeFiles
    case manifestFiles of
      Nothing -> fail "Error"
      (Just (fs :: [Path Rel File])) -> concat <$> traverse (nestedValidatedProjects rootDir) fs

mkProjectClosure :: Path Rel File -> [ValidatedProject] -> ProjectClosureBody
mkProjectClosure file projects = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph projects
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

-- DTD for the Repo manifest.xml file: https://gerrit.googlesource.com/git-repo/+/master/docs/manifest-format.md
-- Note that the DTD is only "roughly adhered to" according to the documentation. For example, the DTD says that
-- there will be zero or more project and remote tags (it uses a `*`), but the documentation specifies at least one
-- for both of these tags (which should be denoted by a `+` in the DTD).
data RepoManifest = RepoManifest
  { manifestDefault  :: Maybe ManifestDefault
  , manifestRemotes  :: [ManifestRemote]
  , manifestProjects :: [ManifestProject]
  , manifestIncludes :: [ManifestInclude]
  } deriving (Eq, Ord, Show, Generic)

data ManifestRemote = ManifestRemote
  { remoteName     :: Text
  , remoteFetch    :: Text
  , remoteRevision :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

data ManifestDefault = ManifestDefault
  { defaultRemote   :: Maybe Text
  , defaultRevision :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

data ManifestProject = ManifestProject
  { projectName     :: Text
  , projectPath     :: Maybe Text
  , projectRemote   :: Maybe Text
  , projectRevision :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

data ManifestInclude = ManifestInclude { includeName :: Text } deriving (Eq, Ord, Show, Generic)

data ValidatedProject = ValidatedProject
  { validatedProjectName     :: Text
  , validatedProjectPath     :: Text
  , validatedProjectUrl      :: URI
  , validatedProjectRevision :: Text
  } deriving (Eq, Ord, Show, Generic)

-- If a project does not have a path, then use its name for the path
projectPathOrName :: ManifestProject -> Text
projectPathOrName ManifestProject { projectPath = Nothing, projectName = name } = name
projectPathOrName ManifestProject { projectPath = Just path } = path

-- A project's revision comes from the first of these that we encounter:
--   * If the project has a revision attribute, then use that
--   * If the project has a remote attribute and the remote it points to has a revision attribute, use that
--   * If the project does not have a remote attribute and the default remote has a revision attribute, use that
--   * Otherwise, use the revision attribute on the project's default tag
--   That leaves these error cases:
--   * If the project does not have a remote attribute and there is no default remote, then blow up
--   * If the project does not have a revision and there is no default revision from either its remote or the default, then blow up
revisionForProject :: RepoManifest -> ManifestProject -> Maybe Text
revisionForProject manifest project =
      projectRevision project
  <|> (remoteForProject manifest project >>= remoteRevision)
  <|> (manifestDefault manifest >>= defaultRevision)

-- The URL for a project is the project's name appended to the fetch attribute of the project's remote
urlForProject :: RepoManifest -> ManifestProject -> Maybe URI
urlForProject manifest project = do
  remote <- remoteForProject manifest project
  remoteUri <- mkURI $ remoteFetch remote
  projectn <- mkURI $ projectName project
  projectn `relativeTo` remoteUri

remoteForProject :: RepoManifest -> ManifestProject -> Maybe ManifestRemote
remoteForProject manifest project =
  remoteNameString >>= remoteByName manifest
  where
    remoteNameString = projectRemote project <|> (manifestDefault manifest >>= defaultRemote)

remoteByName :: RepoManifest -> Text -> Maybe ManifestRemote
remoteByName manifest remoteNameString =
  find (\r -> remoteName r == remoteNameString) (manifestRemotes manifest)

validateProjects :: RepoManifest -> Maybe [ValidatedProject]
validateProjects manifest =
    traverse (validateProject manifest) (manifestProjects manifest)

validateProject :: RepoManifest -> ManifestProject -> Maybe ValidatedProject
validateProject manifest project = do
  revision <- revisionForProject manifest project
  url <- urlForProject manifest project
  pure $ ValidatedProject (projectName project) (projectPathOrName project) url revision

instance FromXML RepoManifest where
  parseElement el =
    RepoManifest <$> optional (child "default" el)
                 <*> children "remote" el
                 <*> children "project" el
                 <*> children "include" el

instance FromXML ManifestDefault where
  parseElement el =
    ManifestDefault <$> optional (attr "remote" el)
                    <*> optional (attr "revision" el)

instance FromXML ManifestRemote where
  parseElement el =
    ManifestRemote <$> attr "name" el
                   <*> attr "fetch" el
                   <*> optional (attr "revision" el)

instance FromXML ManifestProject where
  parseElement el =
    ManifestProject <$> attr "name" el
                    <*> optional (attr "path" el)
                    <*> optional (attr "remote" el)
                    <*> optional (attr "revision" el)

instance FromXML ManifestInclude where
  parseElement el =
    ManifestInclude <$> attr "name" el

buildGraph :: [ValidatedProject] -> Graphing Dependency
buildGraph projects = unfold projects (const []) toDependency
    where
    toDependency ValidatedProject{..} =
      Dependency { dependencyType = GooglesourceType
                 , dependencyName = validatedProjectName
                 , dependencyVersion = Just (CEq validatedProjectRevision)
                 , dependencyLocations = [render validatedProjectUrl]
                 , dependencyTags = M.empty
                 , dependencyEnvironments = [EnvProduction]
                 }

data ManifestGitConfigError =
    InvalidRemote Text
  | GitConfigParse Text
  | MissingGitConfig Text
  | MissingGitHead
  | InvalidBranchName Text
  | MissingBranch Text
  | MissingGitDir
  deriving (Eq, Ord, Show, Generic, Typeable)
