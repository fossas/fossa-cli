{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.API.BuildLink (
  getBuildURLWithOrg,
  getFossaBuildUrl,
  samlUrlPath,
) where

import App.Types (ProjectRevision (..))
import Control.Effect.Diagnostics (Diagnostics, Has, recover)
import Control.Effect.FossaApiClient (
  FossaApiClient,
  getApiOpts,
  getOrganization,
 )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Extra (showT)
import Fossa.API.Types (ApiOpts (..), Organization (..))
import Srclib.Types (Locator (..), projectId)
import Text.URI qualified as URI
import Text.URI.Builder (
  PathComponent (PathComponent),
  Query (Pair),
  TrailingSlash (TrailingSlash),
  renderPath,
  setPath,
  setQuery,
 )
import Text.URI.QQ (uri)

fossaProjectUrlPath :: Locator -> ProjectRevision -> [PathComponent]
fossaProjectUrlPath loc@Locator{..} ProjectRevision{..} = map PathComponent components
  where
    components = ["projects", project, "refs", "branch", branch, revision]
    project = projectId loc
    revision = fromMaybe projectRevision locatorRevision
    -- We default to master because core does, and we need a branch to allow us to
    -- create links directly to builds.  If this changes, then a core change should
    -- be made allowing a link to a known revision on an unknown default branch.
    branch = fromMaybe "master" projectBranch

getFossaBuildUrl :: (Has Diagnostics sig m, Has FossaApiClient sig m) => ProjectRevision -> Locator -> m Text
getFossaBuildUrl revision locator = do
  maybeOrg <- recover getOrganization
  apiOpts <- getApiOpts
  getBuildURLWithOrg maybeOrg revision apiOpts locator

getBuildURLWithOrg :: Has Diagnostics sig m => Maybe Organization -> ProjectRevision -> ApiOpts -> Locator -> m Text
getBuildURLWithOrg maybeOrg revision apiOpts locator = do
  let baseURI = fromMaybe [uri|https://app.fossa.com|] (apiOptsUri apiOpts)
      projectPath = fossaProjectUrlPath locator revision

  (path, query) <- case maybeOrg of
    Just org | orgUsesSAML org -> samlUrlPair org projectPath
    _ -> pure (projectPath, [])
  finaluri <- setPath path (TrailingSlash False) baseURI >>= setQuery query
  pure $ URI.render finaluri

samlUrlPair :: Has Diagnostics sig m => Organization -> [PathComponent] -> m ([PathComponent], [Query])
samlUrlPair org path = do
  pathtext <- renderPath path (TrailingSlash False)
  pure (samlUrlPath org, [Pair "next" pathtext])

samlUrlPath :: Organization -> [PathComponent]
samlUrlPath Organization{organizationId} = map PathComponent ["account", "saml", showT organizationId]
