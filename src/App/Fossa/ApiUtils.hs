module App.Fossa.ApiUtils (
  retrievePolicyId,
  retrieveTeamIds,
  retrieveLabelIds,
  retrieveTeamIdsWithMaybe,
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, errHelp, fatalText)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text, intercalate)
import Effect.Logger (pretty, renderIt, vsep)
import Fossa.API.CoreTypes (Label (..), Labels (..), Policy (..), PolicyType (..), Team (..))

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

retrieveTeamIdsWithMaybe :: Has Diagnostics sig m => Maybe [Text] -> [Team] -> m (Maybe [Int])
retrieveTeamIdsWithMaybe maybeTeamNames teams = case maybeTeamNames of
  Nothing -> pure Nothing
  Just teamNames -> Just <$> retrieveTeamIds teamNames teams

retrieveTeamIds :: Has Diagnostics sig m => [Text] -> [Team] -> m [Int]
retrieveTeamIds teamNames teams = do
  let teamMap = Map.fromList $ map (\team -> (teamName team, teamId team)) teams
      validTeamIds = mapMaybe (`Map.lookup` teamMap) teamNames

  if length teamNames == length validTeamIds
    then pure validTeamIds
    else do
      let missingTeamNames = filter (`Map.notMember` teamMap) teamNames
      fatalText $ "Teams " <> intercalate "," missingTeamNames <> " not found"

retrieveLabelIds :: Has Diagnostics sig m => [Text] -> Labels -> m ([Int], Maybe [Text])
retrieveLabelIds projectLabels (Labels orgLabels) = do
  let orgLabelMap = Map.fromList $ map (\label -> (labelName label, labelId label)) orgLabels
  go orgLabelMap projectLabels []
  where
    go :: Has Diagnostics sig m => Map.Map Text Int -> [Text] -> [Int] -> m ([Int], Maybe [Text])
    go _ [] acc = pure (acc, Nothing)
    go labelMap (x : xs) acc = do
      case Map.lookup x labelMap of
        Just labelId' -> go labelMap xs (labelId' : acc)
        Nothing -> do
          (labelIds, maybeWarnings) <- go labelMap xs acc
          let warning =
                renderIt $
                  vsep
                    [ "Label `" <> pretty x <> "` does not exist"
                    , "Navigate to `Organization Settings` in the FOSSA web UI to create new labels:  https://app.fossa.com/account/settings/organization"
                    ]
          let updatedWarnings = case maybeWarnings of
                Just warnings -> Just (warning : warnings)
                Nothing -> Just [warning]
          pure (labelIds, updatedWarnings)
