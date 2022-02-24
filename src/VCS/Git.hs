module VCS.Git (
  gitLogCmd,
  fetchGitContributors,
) where

import App.Fossa.FossaAPIV1 (Contributors (..))
import Control.Carrier.Diagnostics (errCtx)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Effect.Lift (Lift, sendIO)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.String.Conversion (decodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Extra (splitOnceOn)
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Diag.Diagnostic (ToDiagnostic)
import Effect.Exec
import Path

gitLogCmd :: UTCTime -> Command
gitLogCmd now =
  Command
    { cmdName = "git"
    , cmdArgs = ["log", "--since", sinceArg, "--date=short", "--format=%ae|%cd"]
    , cmdAllowErr = Never
    }
  where
    sinceArg = toText . iso8601Show $ utctDay wayBack
    delta = nominalDay * (-90)
    wayBack = addUTCTime delta now

fetchGitContributors ::
  ( Has Diag.Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs Dir ->
  m Contributors
fetchGitContributors basedir = do
  now <- sendIO getCurrentTime
  rawContrib <- errCtx FailedToPerformGitLog $ execThrow basedir $ gitLogCmd now
  pure . Contributors
    . Map.map (toText . iso8601Show)
    . Map.fromListWith max
    . mapMaybe readLine
    . Text.lines
    $ decodeUtf8 rawContrib
  where
    readLine :: Text -> Maybe (Text, Day)
    readLine entry = do
      let (email, textDate) = splitOnceOn "|" entry
      date <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d" $ toString textDate
      Just (email, date)

data FailedToPerformGitLog = FailedToPerformGitLog
instance ToDiagnostic FailedToPerformGitLog where
  renderDiagnostic _ = "Could not retrieve git logs for contributor counting."
