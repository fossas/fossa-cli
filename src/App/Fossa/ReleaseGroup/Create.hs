{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ReleaseGroup.Create (
  createMain,
  retrievePolicyId,
  retrieveTeamIds,
) where

import App.Fossa.Config.ReleaseGroup.Create (CreateConfig (..))
import App.Fossa.ReleaseGroup.Common (retrieveReleaseGroupId)
import App.Types (ReleaseGroupRevision (..))
import Control.Algebra (Has)
import Control.Carrier.StickyLogger (runStickyLogger)
import Control.Effect.Diagnostics (Diagnostics, context, errHelp, fatalText)
import Control.Effect.FossaApiClient (FossaApiClient, createReleaseGroup, getPolicies, getReleaseGroups, getTeams)
import Control.Effect.Lift (Lift)
import Control.Monad (when)
import Data.Map qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.String.Conversion (ToText (..))
import Data.Text (Text, intercalate)
import Effect.Logger (Logger, Severity (SevDebug, SevInfo), logInfo, logStdout)
import Fossa.API.Types (CreateReleaseGroupRequest (..), CreateReleaseGroupResponse (..), Policy (..), PolicyType (..), Team (..))

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
  maybeTeamIds <- context "Retrieving team IDs" $ retrieveTeamIds (releaseGroupTeams releaseGroupRevision) teams

  let req = CreateReleaseGroupRequest (releaseGroupTitle releaseGroupRevision) (releaseGroupReleaseRevision releaseGroupRevision) maybeLicensePolicyId maybeSecurityPolicyId maybeQualityPolicyId maybeTeamIds
  res <- createReleaseGroup req
  logStdout $ "Created release group with id: " <> toText (groupId res) <> "\n"
  logStdout $ "View the created release group at: " <> createdReleaseGroupLink (groupId res) <> "\n"
  where
    createdReleaseGroupLink :: Int -> Text
    createdReleaseGroupLink releaseGroupId = "https://app.fossa.com/projects/group/" <> toText releaseGroupId

retrievePolicyId :: Has Diagnostics sig m => Maybe Text -> PolicyType -> [Policy] -> m (Maybe Int)
retrievePolicyId maybeTitle targetType policies = case maybeTitle of
  Nothing -> pure Nothing
  Just targetTitle -> do
    let filteredPolicies = filter (\p -> policyTitle p == targetTitle && (policyType p == targetType)) policies
    case filteredPolicies of
      [] -> fatalText $ "Policy `" <> targetTitle <> "` not found"
      [policy] -> pure . Just $ policyId policy
      (_ : _ : _) ->
        errHelp ("Navigate to the FOSSA web UI to rename your policies so that they are unqiue" :: Text)
          . fatalText
          $ "Multiple policies with title `" <> targetTitle <> "` found. Unable to determine which policy to use."

retrieveTeamIds :: Has Diagnostics sig m => Maybe [Text] -> [Team] -> m (Maybe [Int])
retrieveTeamIds maybeTeamNames teams = case maybeTeamNames of
  Nothing -> pure Nothing
  Just teamNames -> do
    let teamMap = Map.fromList $ map (\team -> (teamName team, teamId team)) teams
        validTeamIds = mapMaybe (`Map.lookup` teamMap) teamNames

    if length teamNames == length validTeamIds
      then pure . Just $ validTeamIds
      else do
        let missingTeamNames = filter (`Map.notMember` teamMap) teamNames
        fatalText $ "Teams " <> intercalate "," missingTeamNames <> "not found"
