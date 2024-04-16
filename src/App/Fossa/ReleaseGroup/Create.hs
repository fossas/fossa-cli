{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ReleaseGroup.Create (
  createMain,
) where

import App.Fossa.ApiUtils (retrievePolicyId, retrieveTeamIdsWithMaybe)
import App.Fossa.Config.ReleaseGroup.Create (CreateConfig (..))
import App.Fossa.ReleaseGroup.Common (retrieveReleaseGroupId)
import App.Types (ReleaseGroupRevision (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, errHelp, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, createReleaseGroup, getPolicies, getReleaseGroups, getTeams)
import Control.Effect.Lift (Lift)
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.String.Conversion (ToText (..))
import Data.Text (Text)
import Effect.Logger (Logger, logInfo, logStdout)
import Fossa.API.CoreTypes (CreateReleaseGroupRequest (..), CreateReleaseGroupResponse (..), PolicyType (..))

createMain ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has FossaApiClient sig m
  ) =>
  CreateConfig ->
  m ()
createMain CreateConfig{..} = do
  logInfo "Running FOSSA release-group create"

  releaseGroups <- getReleaseGroups
  maybeReleaseGroupId <- retrieveReleaseGroupId (releaseGroupTitle releaseGroupRevision) releaseGroups
  -- Having release groups with the same name is a current functionality. However, we want to refrain from creating release groups with the same
  -- name on the CLI as it will make it harder to determine which release groups users want to modify in our other release group commands.
  when (isJust maybeReleaseGroupId)
    $ errHelp ("Navigate to the FOSSA web UI to rename your existing release group or create a release group using a different name" :: Text)
      . fatalText
    $ "Release Group `" <> releaseGroupTitle releaseGroupRevision <> "` already exists"

  policies <- getPolicies
  teams <- getTeams

  maybeLicensePolicyId <- context "Retrieving license policy ID" $ retrievePolicyId (releaseGroupLicensePolicy releaseGroupRevision) LICENSING policies
  maybeSecurityPolicyId <- context "Retrieving security policy ID" $ retrievePolicyId (releaseGroupSecurityPolicy releaseGroupRevision) SECURITY policies
  maybeQualityPolicyId <- context "Retrieving quality policy ID" $ retrievePolicyId (releaseGroupQualityPolicy releaseGroupRevision) QUALITY policies
  maybeTeamIds <- context "Retrieving team IDs" $ retrieveTeamIdsWithMaybe (releaseGroupTeams releaseGroupRevision) teams

  let req = CreateReleaseGroupRequest (releaseGroupTitle releaseGroupRevision) (releaseGroupReleaseRevision releaseGroupRevision) maybeLicensePolicyId maybeSecurityPolicyId maybeQualityPolicyId maybeTeamIds
  res <- createReleaseGroup req
  logStdout $ "Created release group with id: " <> toText (groupId res) <> "\n"
  logStdout $ "View the created release group at: " <> createdReleaseGroupLink (groupId res) <> "\n"
  where
    createdReleaseGroupLink :: Int -> Text
    createdReleaseGroupLink releaseGroupId = "https://app.fossa.com/projects/group/" <> toText releaseGroupId
