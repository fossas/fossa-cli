{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Analyze.Log4jReport (
  analyzeForLog4j,

  -- * for testing
  getVulnerableDeps,
  getVulnerability,
  Vulnerability (..),
  SimplifiedVersion (..),
  parseSimplifiedVersion,
) where

import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.Analyze.Types (
  AnalyzeExperimentalPreferences (..),
 )
import Control.Carrier.AtomicCounter (runAtomicCounter)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Finally (Has, runFinally)
import Control.Carrier.StickyLogger (runStickyLogger)
import Control.Concurrent (getNumCapabilities)
import Control.Effect.Lift (sendIO)
import Data.List qualified as List
import Data.Set qualified as Set
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (Dependency (..), VerConstraint)
import Effect.ReadFS (runReadFSIO)

import App.Fossa.Analyze (runAnalyzers, updateProgress)
import App.Types (
  BaseDir (..),
 )
import App.Util (validateDir)
import Control.Carrier.Lift (Lift)
import Control.Carrier.Output.IO (runOutput)
import Control.Carrier.Reader (runReader)
import Control.Carrier.TaskPool (
  withTaskPool,
 )
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Void (Void)
import Discovery.Filters (AllFilters (AllFilters), FilterCombination (FilterCombination))
import Effect.Exec (runExecIO)
import Effect.Logger (
  Logger,
  Severity (SevInfo),
  logStdout,
  renderIt,
  withDefaultLogger,
 )
import Graphing (directList, getRootsOf, hasPredecessors, vertexList)
import Path (File, SomeBase)
import Prettyprinter (Doc, Pretty (pretty), annotate, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Yellow), color)
import Srclib.Converter (verConstraintToRevision)
import Text.Megaparsec
import Text.Read (readMaybe)

type Parser = Parsec Void Text

data SimplifiedVersion = SimplifiedVersion Int Int deriving (Show, Eq, Ord)

parseSimplifiedVersion :: Parser SimplifiedVersion
parseSimplifiedVersion = do
  major <- toString <$> takeWhileP (Just "MajorVersion") isDigitChar
  _ <- chunk "."
  minor <- toString <$> takeWhileP (Just "MinorVersion") isDigitChar
  case (readMaybe major, readMaybe minor) of
    (Just majorVer, Just minorVer) -> pure $ SimplifiedVersion majorVer minorVer
    _ -> fail ("failed to parse " <> show (major, minor))
  where
    isDigitChar c = c `elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

-- | Performs Analysis for Log4j, and prints report detailing projects using log4j.
analyzeForLog4j ::
  FilePath ->
  IO ()
analyzeForLog4j targetDirectory = do
  basedir <- sendIO $ validateDir targetDirectory
  capabilities <- sendIO getNumCapabilities

  withDefaultLogger SevInfo
    . Diag.logWithExit_
    . runReadFSIO
    . runReader withoutAnyExperimentalPreferences
    . runExecIO
    . ignoreDebug
    $ do
      (projectResults, ()) <-
        Diag.context "discovery/analysis tasks"
          . runOutput @ProjectResult
          . runStickyLogger SevInfo
          . runFinally
          . withTaskPool capabilities updateProgress
          . runAtomicCounter
          $ do
            runAnalyzers (toPath basedir) withoutFilters
      reportLog4jVulnerability projectResults
  where
    toPath (BaseDir path) = path
    withoutAnyExperimentalPreferences = AnalyzeExperimentalPreferences Nothing
    withoutFilters = AllFilters [] (FilterCombination [] []) (FilterCombination [] [])

data VulnerableDependency = VulnerableDependency
  { vdName :: Text
  , vdKnownVulnerableVersion :: Map.Map SimplifiedVersion (Set Vulnerability)
  }
  deriving (Show, Eq, Ord)

data Vulnerability
  = VulnerabilityRemoteCodeExecution
  | VulnerabilityOther
  deriving (Eq, Ord)

instance Show Vulnerability where
  show (VulnerabilityRemoteCodeExecution) = "Vulnerable - RemoteCodeExecution"
  show (VulnerabilityOther) = "Vulnerable"

getVulnerableDeps :: [VulnerableDependency]
getVulnerableDeps =
  [ VulnerableDependency "org.apache.logging.log4j:log4j-core" $
      Map.fromList
        [ (SimplifiedVersion 2 0, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 1, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 2, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 3, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 4, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 5, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 6, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 7, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 8, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 9, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 10, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 11, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 12, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 13, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 14, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 15, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 16, Set.singleton VulnerabilityOther)
        ]
  , VulnerableDependency
      "org.apache.logging.log4j:log4j"
      $ Map.fromList
        [ (SimplifiedVersion 2 0, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 1, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 2, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 3, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 4, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 5, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 6, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 7, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 8, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 9, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 10, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 11, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 12, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 13, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 14, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 15, Set.singleton VulnerabilityRemoteCodeExecution)
        , (SimplifiedVersion 2 16, Set.singleton VulnerabilityOther)
        ]
  , VulnerableDependency "log4j:log4j" $
      Map.fromList
        [ (SimplifiedVersion 1 0, Set.singleton VulnerabilityOther)
        , (SimplifiedVersion 1 1, Set.singleton VulnerabilityOther)
        , (SimplifiedVersion 1 2, Set.singleton VulnerabilityOther)
        ]
  ]

data Log4jVulnerableReportItem = Log4jVulnerableReportItem
  { vriName :: Text
  , vriVersion :: Maybe VerConstraint
  , vriOrigin :: VulnerableDependencyOrigin
  , vriVulnerability :: Set Vulnerability
  }
  deriving (Show, Eq, Ord)

data VulnerableDependencyOrigin
  = VulnerableDependencyManifestFiles [SomeBase File]
  | VulnerableDependencyRootDependency [Dependency]
  deriving (Eq, Ord)

inBracket :: Text -> Text
inBracket t = "(" <> t <> ")"

instance Show VulnerableDependencyOrigin where
  show (VulnerableDependencyManifestFiles files) = toString $ "via manifest: " <> Text.intercalate ", " (map (toText . show) files)
  show (VulnerableDependencyRootDependency deps) = toString $ "via dependencies:" <> inBracket (Text.intercalate ", " $ map dependencyName deps)

annotateWarn :: Doc AnsiStyle -> Doc AnsiStyle
annotateWarn = annotate (color Yellow)

reportLog4jVulnerability :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) => [ProjectResult] -> m ()
reportLog4jVulnerability projects = do
  logStdout $
    renderIt
      ( vsep
          [ ""
          , "-----------------------------------------"
          , "Log4j Dependencies & Vulnerability Report"
          , "-----------------------------------------"
          , "Lists all projects, reports any log4j dependencies and it's vulnerability type (if any)."
          , ""
          , "For more information refer to:"
          , "- Log4j Vulnerability: https://fossa.com/blog/log4j-log4shell-zero-day-vulnerability-impact-fixes/"
          , "- Fossa CLI documentation: https://github.com/fossas/fossa-cli/blob/master/README.md"
          , ""
          , "To analyze all dependencies (not only log4j), please refer to Fossa CLI."
          , ""
          ]
      )
  projectReports <- traverse printProjectReport projects
  logStdout $ renderIt (vsep projectReports)

getVulnerability :: Dependency -> Set Vulnerability
getVulnerability Dependency{..} =
  case List.find (\vd -> dependencyName == vdName vd) getVulnerableDeps of
    Nothing -> Set.empty
    Just vd -> case (verConstraintToRevision =<< dependencyVersion) of
      Nothing -> Set.empty
      Just versionText -> case parse parseSimplifiedVersion "dependency version" versionText of
        Left _ -> Set.empty
        Right sv -> Map.findWithDefault Set.empty sv (vdKnownVulnerableVersion vd)

printProjectReport :: (Has Diag.Diagnostics sig m) => ProjectResult -> m (Doc AnsiStyle)
printProjectReport ProjectResult{..} =
  pure $
    vsep
      [ ""
      , "Project Path: " <> (pretty . show $ projectResultPath)
      , pretty $ Text.intercalate "" $ replicate (length $ "Project Path: " <> (show projectResultPath)) "-"
      , listPretty "Project Manifest Files" (pretty . show <$> projectResultManifestFiles)
      , listPretty "Direct Inclusions" (map withAnnotation directInclusion)
      , listPretty "Indirect Inclusions" (map withAnnotation inDirectInclusion)
      ]
  where
    listPretty :: Text -> [Doc AnsiStyle] -> Doc AnsiStyle
    listPretty header items =
      if null items
        then (pretty (header <> ": ")) <> "0 log4j dependencies found!"
        else vsep $ (pretty header <> ":") : map ("- " <>) items

    directInclusion :: [Log4jVulnerableReportItem]
    directInclusion = map (toVulnerableReportItem toVulnerableDependencyManifestFiles) onlyDirects

    inDirectInclusion :: [Log4jVulnerableReportItem]
    inDirectInclusion = map (toVulnerableReportItem toVulnerableDependencyRootDependency) allDeep

    withAnnotation :: Log4jVulnerableReportItem -> Doc AnsiStyle
    withAnnotation (Log4jVulnerableReportItem name version origin vulTypes) =
      colorCoded $
        Text.intercalate
          " "
          [ name
          , formattedVersion
          , formattedVulnerability
          , toText . show $ origin
          ]
      where
        colorCoded doc = if Set.null vulTypes then (pretty doc) else (annotateWarn $ pretty doc)
        formattedVersion = maybe "N/A" ("v" <>) (verConstraintToRevision =<< version)
        formattedVulnerability =
          if Set.null vulTypes
            then inBracket "N/A"
            else inBracket $ Text.intercalate ", " (map (toText . show) $ Set.toList vulTypes)

    onlyDirects :: [Dependency]
    onlyDirects = filter isRelevantDep $ directList projectResultGraph

    -- Note, in our model dependency can be direct AND deep at the same time.
    -- Dependency is *deep* only, when it has at-least one predecessor.
    allDeep :: [Dependency]
    allDeep = filter isRelevantDep $ filter (hasPredecessors projectResultGraph) $ vertexList projectResultGraph

    isRelevantDep :: Dependency -> Bool
    isRelevantDep Dependency{..} =
      any
        (\vd -> dependencyName == vdName vd)
        getVulnerableDeps

    toVulnerableDependencyManifestFiles :: Dependency -> VulnerableDependencyOrigin
    toVulnerableDependencyManifestFiles _ = VulnerableDependencyManifestFiles projectResultManifestFiles

    toVulnerableDependencyRootDependency :: Dependency -> VulnerableDependencyOrigin
    toVulnerableDependencyRootDependency dep = VulnerableDependencyRootDependency (getRootsOf projectResultGraph dep)

    toVulnerableReportItem :: (Dependency -> VulnerableDependencyOrigin) -> Dependency -> Log4jVulnerableReportItem
    toVulnerableReportItem originGetter dep =
      Log4jVulnerableReportItem
        { vriName = dependencyName dep
        , vriVersion = dependencyVersion dep
        , vriOrigin = originGetter dep
        , vriVulnerability = getVulnerability dep
        }
