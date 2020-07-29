module VCS.Git
  ( gitLogCmd,
    fetchGitContributors,
  )
where

import App.Fossa.FossaAPIV1 (Contributors (..))
import qualified Control.Carrier.Diagnostics as Diag
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Effect.Exec
import Prologue

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
    MonadIO m
  ) =>
  Path x Dir ->
  m Contributors
fetchGitContributors basedir = do
  now <- liftIO getCurrentTime
  rawContrib <- execThrow basedir $ gitLogCmd now
  textContrib <- Diag.fromEitherShow . TE.decodeUtf8' $ toStrict rawContrib
  pure . Contributors
    . M.map (T.pack . iso8601Show)
    . M.fromListWith max
    . mapMaybe readLine
    $ T.lines textContrib
  where
    readLine :: Text -> Maybe (Text, Day)
    readLine entry = do
      let (email, textDate) = splitOnceOn "|" entry
      date <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d" $ T.unpack textDate
      Just (email, date)

splitOnceOn :: Text -> Text -> (Text, Text)
splitOnceOn needle haystack = (head, strippedTail)
  where
    len = T.length needle
    (head, tail) = T.breakOn needle haystack
    strippedTail = T.drop len tail
