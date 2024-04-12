module App.Fossa.ReleaseGroup.Common (
  retrieveReleaseGroupId,
  retrieveReleaseGroupRelease,
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, errHelp, fatalText)
import Data.Text (Text)
import Fossa.API.Types (ReleaseGroup (..), ReleaseGroupRelease (..))

retrieveReleaseGroupId :: Has Diagnostics sig m => Text -> [ReleaseGroup] -> m (Maybe Int)
retrieveReleaseGroupId title releaseGroups = do
  let filteredReleaseGroups = filter (\rg -> releaseGroupTitle rg == title) releaseGroups
  case filteredReleaseGroups of
    [] -> pure Nothing
    [releaseGroup] -> pure . Just $ releaseGroupId releaseGroup
    (_ : _ : _) ->
      errHelp ("Navigate to the FOSSA web UI to rename your release groups so that they are unique." :: Text)
        . fatalText
        $ "Multiple instances of release groups with title `" <> title <> "` found. Unable to determine which release group to modify."

retrieveReleaseGroupRelease :: Has Diagnostics sig m => Text -> [ReleaseGroupRelease] -> m ReleaseGroupRelease
retrieveReleaseGroupRelease releaseTitle releases = do
  let filteredReleaseGroups = filter (\r -> releaseGroupReleaseTitle r == releaseTitle) releases
  case filteredReleaseGroups of
    [] -> fatalText $ "Release `" <> releaseTitle <> "` not found"
    [release] -> pure release
    (_ : _ : _) ->
      errHelp ("Navigate to the FOSSA web UI to rename your release group releases so that they are unique." :: Text)
        . fatalText
        $ "Multiple instances of releases with title `" <> releaseTitle <> "` found. Unable to determine which release to modify."
