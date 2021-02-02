{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.API.BuildLink
  ( getFossaBuildUrl,
    samlUrlPath,
  )
where

import App.Fossa.FossaAPIV1 (Organization (..), getOrganization)
import App.Types
import Control.Effect.Diagnostics hiding (fromMaybe)
import Control.Effect.Lift
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Extra (showT, underBS)
import Fossa.API.Types (ApiOpts (..))
import Network.HTTP.Types (urlEncode)
import Srclib.Types (Locator (..))
import qualified Text.URI as URI

fossaProjectUrlPath :: Locator -> ProjectRevision -> Text
fossaProjectUrlPath Locator {..} ProjectRevision {..} = "/projects/" <> encodedProject <> buildSelector
  where
    encodedProject = urlEncode' (locatorFetcher <> "+" <> locatorProject)
    encodedRevision = urlEncode' $ fromMaybe projectRevision locatorRevision
    -- We default to master because core does, and we need a branch to allow us to
    -- create links directly to builds.  If this changes, then a core change should
    -- be made allowing a link to a known revision on an unknown default branch.
    encodedBranch = urlEncode' $ fromMaybe "master" projectBranch
    buildSelector = "/refs/branch/" <> encodedBranch <> "/" <> encodedRevision

getFossaBuildUrl :: (Has Diagnostics sig m, Has (Lift IO) sig m) => ProjectRevision -> ApiOpts -> Locator -> m Text
getFossaBuildUrl revision apiopts locator = do
  maybeOrg <- recover $ getOrganization apiopts

  let baseURI = apiOptsUri apiopts
      relUriPath = case maybeOrg of
        Just org | orgUsesSAML org -> samlUrlPath org locator revision
        _ -> fossaProjectUrlPath locator revision
  pure (URI.render baseURI <> relUriPath)

samlUrlPath :: Organization -> Locator -> ProjectRevision -> Text
samlUrlPath Organization {organizationId} locator revision = "account/saml/" <> showT organizationId <> "?" <> opts
  where
    opts = "next=" <> urlEncode' redirectPath
    redirectPath = fossaProjectUrlPath locator revision

urlEncode' :: Text -> Text
urlEncode' = underBS (urlEncode True)