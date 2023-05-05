{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Scan.RunWiggins (
  execWiggins,
  generateWigginsMonorepoOpts,
  toPathFilters,
  WigginsOpts (..),
  PathFilters (..),
) where

import App.Fossa.EmbeddedBinary (BinaryPaths, toPath)
import App.Types (
  MonorepoAnalysisOpts (..),
  ProjectMetadata (..),
  ProjectRevision (..), Policy (..),
 )
import Control.Carrier.Error.Either (Has)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic (renderDiagnostic))
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Discovery.Filters (AllFilters (..), combinedPaths)
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  execThrow,)
import Effect.Logger (Severity (SevDebug))
import Fossa.API.Types (ApiKey (unApiKey), ApiOpts (..))
import Path (Abs, Dir, Path, Rel, fromAbsFile)
import Text.URI (render)
import Prettyprinter (pretty)

data WigginsOpts = WigginsOpts
  { scanDir :: Path Abs Dir
  , spectrometerArgs :: [Text]
  }

data PathFilters = PathFilters
  { onlyPaths :: [Path Rel Dir]
  , excludePaths :: [Path Rel Dir]
  }

toPathFilters :: AllFilters -> PathFilters
toPathFilters AllFilters{includeFilters, excludeFilters} = PathFilters (combinedPaths includeFilters) (combinedPaths excludeFilters)

data MonorepoPolicyIdError = MonorepoPolicyIdError
  deriving (Eq, Ord, Show)

instance ToDiagnostic MonorepoPolicyIdError where
  renderDiagnostic _ = pretty @Text "The --policy-id or policyId directives are not supported for monorepo scans. Check your .fossa.yml or fossa invocation."

generateWigginsMonorepoOpts :: Path Abs Dir -> MonorepoAnalysisOpts -> PathFilters -> Severity -> ProjectRevision -> ApiOpts -> ProjectMetadata -> Either MonorepoPolicyIdError WigginsOpts
generateWigginsMonorepoOpts scanDir monorepoAnalysisOpts filters logSeverity projectRevision apiOpts metadata =
  WigginsOpts scanDir <$> generateMonorepoArgs monorepoAnalysisOpts filters logSeverity projectRevision apiOpts metadata

-- A project config containing a PolicyId should be rejected by the caller.
-- This function will totally ignore policy ids because wiggins doesn't support it.
generateMonorepoArgs :: MonorepoAnalysisOpts -> PathFilters -> Severity -> ProjectRevision -> ApiOpts -> ProjectMetadata -> Either MonorepoPolicyIdError [Text]
generateMonorepoArgs MonorepoAnalysisOpts{..} PathFilters{..} logSeverity ProjectRevision{..} ApiOpts{..} ProjectMetadata{..} =
  case projectPolicy of
    Just (PolicyId _) -> Left MonorepoPolicyIdError
    Just (PolicyName n) -> Right $ policyArgs (Just n)
    Nothing -> Right $ policyArgs Nothing
    where policyArgs p = 
            "monorepo"
            : optMaybeText "-endpoint" (render <$> apiOptsUri)
             ++ ["-fossa-api-key", unApiKey apiOptsApiKey]
             ++ ["-project", projectName, "-revision", projectRevision]
             ++ optMaybeText "-jira-project-key" projectJiraKey
             ++ optMaybeText "-link" projectLink
             ++ optMaybeText "-policy" p
             ++ optMaybeText "-project-url" projectUrl
             ++ optMaybeText "-team" projectTeam
             ++ optMaybeText "-title" projectTitle
             ++ optMaybeText "-branch" projectBranch
             ++ optExplodeText "-only-path" (optPathAsFilter <$> onlyPaths)
             ++ optExplodeText "-exclude-path" (optPathAsFilter <$> excludePaths)
             ++ optBool "-debug" (logSeverity == SevDebug)
             ++ optMaybeText "-type" monorepoAnalysisType
             ++ ["."]
          

optBool :: Text -> Bool -> [Text]
optBool flag True = [flag]
optBool _ False = []

optMaybeText :: Text -> Maybe Text -> [Text]
optMaybeText _ Nothing = []
optMaybeText flag (Just value) = [flag, value]

optExplodeText :: Text -> [Text] -> [Text]
optExplodeText _ [] = []
optExplodeText flag [a] = [flag, a]
optExplodeText flag (a : as) = [flag, a] ++ optExplodeText flag as

-- Path Rel Dir renders with a trailing /, but wiggins needs it to not have that trailing slash when used as an exclude or include-only filter.
-- Strip the / postfix from the returned value.
optPathAsFilter :: Path Rel Dir -> Text
optPathAsFilter p = fromMaybe t $ Text.stripSuffix "/" t
  where
    t = toText p

execWiggins :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m Text
execWiggins binaryPaths opts = decodeUtf8 . BL.toStrict <$> execThrow (scanDir opts) (wigginsCommand binaryPaths opts)

wigginsCommand :: BinaryPaths -> WigginsOpts -> Command
wigginsCommand bin WigginsOpts{..} = do
  Command
    { cmdName = toText $ fromAbsFile $ toPath bin
    , cmdArgs = spectrometerArgs
    , cmdAllowErr = Never
    }
