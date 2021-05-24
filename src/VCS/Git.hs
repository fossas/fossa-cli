module VCS.Git
  ( gitLogCmd,
    fetchGitContributors,
  )
where

import App.Fossa.FossaAPIV1 (Contributors (..))
import qualified Control.Carrier.Diagnostics as Diag
import Control.Effect.Lift (Lift, sendIO)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Extra (splitOnceOn)
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Effect.Exec
import Path

gitLogCmd :: UTCTime -> Command
gitLogCmd now =
  Command
    { cmdName = "git",
      cmdArgs = ["log", "--since", sinceArg, "--date=short", "--format=%ae|%cd"],
      cmdAllowErr = Never
    }
  where
    sinceArg = T.pack . iso8601Show $ utctDay wayBack
    delta = nominalDay * (-90)
    wayBack = addUTCTime delta now

fetchGitContributors ::
  ( Has Diag.Diagnostics sig m,
    Has Exec sig m,
    Has (Lift IO) sig m
  ) =>
  Path x Dir ->
  m Contributors
fetchGitContributors basedir = do
  now <- sendIO getCurrentTime
  rawContrib <- execThrow basedir $ gitLogCmd now
  pure . Contributors
    . M.map (T.pack . iso8601Show)
    . M.fromListWith max
    . mapMaybe readLine
    . T.lines
    $ decodeUtf8 rawContrib
  where
    readLine :: Text -> Maybe (Text, Day)
    readLine entry = do
      let (email, textDate) = splitOnceOn "|" entry
      date <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d" $ T.unpack textDate
      Just (email, date)
