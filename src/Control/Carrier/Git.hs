{-# LANGUAGE GADTs #-}

module Control.Carrier.Git (
  GitC,
  runGit,
) where

import Control.Carrier.Simple (SimpleC, interpret)
import Control.Effect.Diagnostics (
  ToDiagnostic (renderDiagnostic),
  errCtx,
 )
import Control.Effect.Diagnostics qualified as Diag
import Control.Effect.Git (GitF (..))
import Control.Effect.Lift (Lift, sendIO)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.String.Conversion (decodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Extra (splitOnceOn)
import Data.Time (
  Day,
  NominalDiffTime,
  UTCTime (utctDay),
  addUTCTime,
  defaultTimeLocale,
  getCurrentTime,
  nominalDay,
  parseTimeM,
 )
import Data.Time.Format.ISO8601 (iso8601Show)
import Diag.Diagnostic (DiagnosticInfo (..))
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  Has,
  execThrow,
 )
import Fossa.API.Types (Contributors (..))
import Path (Abs, Dir, Path)

type GitC = SimpleC GitF

gitContributorDays :: NominalDiffTime
gitContributorDays = 365

runGit :: (Has (Lift IO) sig m, Has Exec sig m, Has Diag.Diagnostics sig m) => GitC m a -> m a
runGit = interpret $ \case
  FetchGitContributors baseDir -> fetchGitContributors baseDir

gitLogCmd :: UTCTime -> Command
gitLogCmd now =
  Command
    { cmdName = "git"
    , cmdArgs = ["log", "--since", sinceArg, "--date=short", "--format=%ae|%cd"]
    , cmdAllowErr = Never
    }
  where
    sinceArg = toText . iso8601Show $ utctDay wayBack
    delta = nominalDay * (-gitContributorDays)
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
  pure
    . Contributors
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
  renderDiagnostic _ = do
    let ctx = "Could not retrieve git logs for contributor counting."
    DiagnosticInfo Nothing Nothing Nothing Nothing Nothing (Just ctx) Nothing