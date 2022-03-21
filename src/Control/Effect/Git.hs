module Control.Effect.Git (GitF (..), Git, fetchGitContributors) where

import Control.Carrier.Simple (Has, Simple, sendSimple)
import Fossa.API.Types (Contributors)
import Path (Abs, Dir, Path)

data GitF a where
  FetchGitContributors :: Path Abs Dir -> GitF Contributors

type Git = Simple GitF

fetchGitContributors :: (Has Git sig m) => Path Abs Dir -> m Contributors
fetchGitContributors = sendSimple . FetchGitContributors
