module Strategy.R.Renv (
  analyzeLockFile,
  analyzeDescription,
  analyzeDescriptionAndLockFile,

  -- * for testing
  RenvLock (..),
  RenvLockRepository (..),
  RenvLockPackage (..),
  RenvPackageSource (..),
  buildGraph,
  toDependency,
) where

import Control.Effect.Diagnostics (Diagnostics, Has)
import Data.Aeson (
  FromJSON (parseJSON),
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson.Types (Key, Object, Parser)
import Data.Foldable (for_)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (toList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepType (CranType, GitType, URLType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Grapher (deep, direct, edge, evalGrapher, run)
import Effect.ReadFS (ReadFS, readContentsJson, readContentsParser)
import Graphing (Graphing)
import Path (Abs, File, Path)
import Strategy.R.Description (RDescription (RDescription), allPkgNames, descriptionParser)
import Types (GraphBreadth (Complete, Partial))

-- | Represents lockfile of: https://github.com/rstudio/renv.
-- ref: https://rstudio.github.io/renv/articles/lockfile.html
data RenvLock = RenvLock
  { repositories :: [RenvLockRepository]
  , packages :: Map Text RenvLockPackage
  }
  deriving (Show, Eq, Ord)

instance FromJSON RenvLock where
  parseJSON = withObject "RenvLock" $ \obj ->
    RenvLock
      <$> (obj .: "R" |> "Repositories")
      <*> (obj .: "Packages")
    where
      (|>) :: FromJSON a => Parser Object -> Key -> Parser a
      (|>) parser key = do
        obj <- parser
        obj .: key

data RenvLockRepository = RenvLockRepository
  { repoName :: Text
  , repoUrl :: Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON RenvLockRepository where
  parseJSON = withObject "RenvLockRepository" $ \o ->
    RenvLockRepository
      <$> (o .: "Name")
      <*> (o .: "URL")

data RenvLockPackage = RenvLockPackage
  { pkgName :: Text
  , pkgVersion :: Text
  , pkgRequirements :: [Text]
  , pkgSource :: RenvPackageSource
  }
  deriving (Show, Eq, Ord)

-- | Package source.
-- ref: https://github.com/rstudio/renv/blob/e7c288f83927fba23aa33fbefd858aa462d19ac1/R/remotes.R#L71
data RenvPackageSource
  = RemoteCran Text
  | Git Text Text
  | Github Text Text Text
  | Gitlab Text Text Text
  | Bitbucket Text Text Text
  | Url Text
  | Repository Text
  | Unknown
  deriving (Show, Eq, Ord)

instance FromJSON RenvLockPackage where
  parseJSON = withObject "RenvLockPackage" $ \o -> do
    pkg <- o .: "Package"
    pkgVersion <- o .: "Version"
    pkgRequirements <- o .:? "Requirements" .!= []

    pkgSource' :: Text <- o .: "Source"
    pkgSource <- case pkgSource' of
      "Bioconductor" ->
        Git
          <$> (o .: "git_url")
          <*> (o .: "git_branch")
      "git" ->
        Git
          <$> (o .: "RemoteUrl")
          <*> (o .: "RemoteRef")
      "GitHub" ->
        Github
          <$> (o .: "RemoteUsername")
          <*> (o .: "RemoteRepo")
          <*> (o .: "RemoteSha")
      "Gitlab" ->
        Gitlab
          <$> (o .: "RemoteUsername")
          <*> (o .: "RemoteRepo")
          <*> (o .: "RemoteSha")
      "Bitbucket" ->
        Bitbucket
          <$> (o .: "RemoteUsername")
          <*> (o .: "RemoteRepo")
          <*> (o .: "RemoteSha")
      "URL" ->
        Url
          <$> (o .: "RemoteUrl")
      "Repository" -> do
        -- some times remote repository referenced
        -- in the 'repositories' are not used for
        -- resolving, this behavior depends on the
        -- API of renv user used.
        --
        -- refer to:
        --  https://github.com/rstudio/renv/issues/120#issue-474858634
        -- -
        pkgRepository <- o .: "Repository"
        remoteRepo <- o .:? "RemoteRepos"

        -- if it is repository sourced, and user
        -- prefer remore repos attribute, if any.
        pure $ case remoteRepo of
          Just rr ->
            if Text.isPrefixOf "http" rr
              then RemoteCran rr
              else Repository pkgRepository
          Nothing -> Repository pkgRepository
      _ -> pure Unknown

    pure $
      RenvLockPackage
        pkg
        pkgVersion
        pkgRequirements
        pkgSource

-- | Builds dependency graph.
-- ref: https://github.com/rstudio/renv/issues/474#issuecomment-656252759
buildGraph :: RDescription -> RenvLock -> Graphing Dependency
buildGraph description renvLock = run . evalGrapher $ do
  let allLockPackages = packages renvLock
  let allRepositories = repositories renvLock
  let allDirectPkgs = allPkgNames description

  if null $ Map.toList allLockPackages
    then
      -- We have empty lock file, and only description manifest
      -- create direct dependencies
      for_ (toList allDirectPkgs) $ \pkgReq -> do
        direct $ toCranDependency pkgReq
    else
      -- We have non-empty lock file, and description manifest
      -- create complete dependency graph. Any dependency that
      -- is mentioned in description is considered to be direct
      for_ (Map.toList allLockPackages) $ \(_, pkgMetadata) -> do
        let currentDep = toDependency allRepositories pkgMetadata
        let childDeps =
              map (toDependency allRepositories) $
                mapMaybe (`Map.lookup` allLockPackages) (pkgRequirements pkgMetadata)

        if Set.member (pkgName pkgMetadata) allDirectPkgs
          then direct currentDep
          else deep currentDep

        for_ childDeps $
          edge currentDep

toCranDependency :: Text -> Dependency
toCranDependency pkg = Dependency CranType pkg Nothing mempty mempty mempty

toDependency :: [RenvLockRepository] -> RenvLockPackage -> Dependency
toDependency repos pkg =
  Dependency
    { dependencyType = depType
    , dependencyName = depName
    , dependencyVersion = CEq <$> depVersion
    , dependencyLocations = mempty
    , dependencyEnvironments = mempty
    , dependencyTags = mempty
    }
  where
    depType :: DepType
    depType = case pkgSource pkg of
      Git{} -> GitType
      Github{} -> GitType
      Gitlab{} -> GitType
      Bitbucket{} -> GitType
      Url{} -> URLType
      RemoteCran{} -> CranType
      Repository{} -> CranType
      Unknown -> CranType

    depName :: Text
    depName = case pkgSource pkg of
      Git url _ -> url
      Github user repo _ -> "https://github.com/" <> user <> "/" <> repo <> ".git"
      Gitlab user repo _ -> "https://gitlab.com/" <> user <> "/" <> repo <> ".git"
      Bitbucket user repo _ -> "https://bitbucket.com/" <> user <> "/" <> repo <> ".git"
      Url url -> url
      RemoteCran url -> url <> ":" <> pkgName pkg
      Repository repoSpecified ->
        case find (\repo -> repoName repo == repoSpecified) (repos) of
          Just rr -> repoUrl rr <> ":" <> pkgName pkg
          Nothing -> pkgName pkg
      Unknown -> pkgName pkg

    depVersion :: Maybe Text
    depVersion = case pkgSource pkg of
      Git _ ref -> Just ref
      Github _ _ ref -> Just ref
      Gitlab _ _ ref -> Just ref
      Bitbucket _ _ ref -> Just ref
      Url _ -> Nothing
      _ -> Just $ pkgVersion pkg

analyzeLockFile ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs File ->
  m (Graphing Dependency, GraphBreadth)
analyzeLockFile lockFilePath = do
  lock <- readContentsJson lockFilePath
  pure
    ( buildGraph emptyDescription lock
    , Partial
    )
  where
    emptyDescription :: RDescription
    emptyDescription = RDescription mempty mempty mempty mempty mempty

analyzeDescriptionAndLockFile ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs File ->
  Path Abs File ->
  m (Graphing Dependency, GraphBreadth)
analyzeDescriptionAndLockFile descriptionFile lockFilePath = do
  description <- readContentsParser descriptionParser descriptionFile
  lock <- readContentsJson lockFilePath
  pure
    ( buildGraph description lock
    , Complete
    )

analyzeDescription ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs File ->
  m (Graphing Dependency, GraphBreadth)
analyzeDescription descriptionFile = do
  description <- readContentsParser descriptionParser descriptionFile
  pure
    ( buildGraph description emptyLockFile
    , Partial
    )
  where
    emptyLockFile :: RenvLock
    emptyLockFile = RenvLock mempty mempty
