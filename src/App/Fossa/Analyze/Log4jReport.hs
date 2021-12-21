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
  AnalyzeTaskEffs,
 )
import Control.Carrier.AtomicCounter (AtomicCounter, runAtomicCounter)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Finally (Has, runFinally)
import Control.Carrier.StickyLogger (runStickyLogger)
import Control.Concurrent (getNumCapabilities)
import Control.Effect.Lift (sendIO)
import Data.List qualified as List
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (Dependency (..), VerConstraint)
import Effect.ReadFS (runReadFSIO)
import Strategy.Gradle qualified as Gradle
import Strategy.Leiningen qualified as Leiningen
import Strategy.Maven qualified as Maven
import Strategy.Scala qualified as Scala

import App.Fossa.Analyze (DiscoverFunc (DiscoverFunc), runDependencyAnalysis, updateProgress)
import App.Types (
  BaseDir (..),
 )
import App.Util (validateDir)
import Control.Carrier.Lift (Lift)
import Control.Carrier.Output.IO (runOutput)
import Control.Carrier.Reader (runReader)
import Control.Carrier.TaskPool (
  TaskPool,
  withTaskPool,
 )
import Control.Effect.Output (Output)
import Data.Foldable (traverse_)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Void (Void)
import Discovery.Filters (AllFilters (AllFilters), FilterCombination (FilterCombination))
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec (runExecIO)
import Effect.Logger (
  Logger,
  Severity (SevInfo),
  logStdout,
  renderIt,
  withDefaultLogger,
 )
import Graphing (directList, getRootsOf, hasPredecessors, vertexList)
import Path
import Prettyprinter (Doc, Pretty (pretty), annotate, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Yellow), color)
import Srclib.Converter (verConstraintToRevision)
import Text.Megaparsec (Parsec, parse)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

data SimplifiedVersion = SimplifiedVersion Int Int deriving (Show, Eq, Ord)

parseSimplifiedVersion :: Parser SimplifiedVersion
parseSimplifiedVersion = SimplifiedVersion <$> decimal <* char '.' <*> decimal

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
            runAnalyzersForLog4j (toPath basedir) withoutFilters
      reportLog4jVulnerability projectResults
  where
    toPath (BaseDir path) = path
    withoutAnyExperimentalPreferences = AnalyzeExperimentalPreferences Nothing
    withoutFilters = AllFilters [] (FilterCombination [] []) (FilterCombination [] [])

runAnalyzersForLog4j ::
  ( AnalyzeTaskEffs sig m
  , Has (Output ProjectResult) sig m
  , Has TaskPool sig m
  , Has AtomicCounter sig m
  ) =>
  Path Abs Dir ->
  AllFilters ->
  m ()
runAnalyzersForLog4j basedir filters = do
  traverse_
    single
    [ DiscoverFunc Gradle.discover
    , DiscoverFunc Leiningen.discover
    , DiscoverFunc Maven.discover
    , DiscoverFunc Scala.discover
    ]
  where
    single (DiscoverFunc f) = withDiscoveredProjects f basedir (runDependencyAnalysis basedir filters)

data VulnerableDependency = VulnerableDependency
  { vdName :: Text
  , vdKnownVulnerableVersion :: Map.Map SimplifiedVersion Vulnerability
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
  [ mkVulnDep corename coreVulns
  , mkVulnDep orgname orgVulns
  , mkVulnDep legacyname legacyVulns
  ]
  where
    corename = "org.apache.logging.log4j:log4j-core"
    orgname = "org.apache.logging.log4j:log4j"
    legacyname = "log4j:log4j"
    mkVulnDep name pairs = VulnerableDependency name $ Map.fromList pairs
    orgVulns = coreVulns -- Same vuln set
    legacyVulns = map (toOtherPair 1) [0 .. 2]
    coreVulns = (toOtherPair 2 16) : map (toRCEPair 2) [0 .. 15]
    toRCEPair major minor = (SimplifiedVersion major minor, VulnerabilityRemoteCodeExecution)
    toOtherPair major minor = (SimplifiedVersion major minor, VulnerabilityOther)

data Log4jVulnerableReportItem = Log4jVulnerableReportItem
  { vriName :: Text
  , vriVersion :: Maybe VerConstraint
  , vriOrigin :: VulnerableDependencyOrigin
  , vriVulnerability :: Maybe Vulnerability
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
          , "For details about the Log4j vulnerability, including mitigation steps, see our"
          , "blog post here: https://fossa.com/blog/log4j-log4shell-zero-day-vulnerability-impact-fixes/"
          , ""
          , "This report is limited to just Log4j, but FOSSA CLI can help find security"
          , "vulnerabilities in your other dependencies too. Sign up for an account at"
          , "https://fossa.com/"
          , ""
          , "Note: This report is experimental, and it may get modified or removed in the future."
          , ""
          , ""
          ]
      )
  projectReports <- traverse printProjectReport projects
  logStdout $ renderIt (vsep projectReports)

getVulnerability :: Dependency -> Maybe Vulnerability
getVulnerability Dependency{..} = do
  vd <- List.find (\vd -> dependencyName == vdName vd) getVulnerableDeps
  versionText <- verConstraintToRevision =<< dependencyVersion
  case (parse parseSimplifiedVersion "dependency version" versionText) of
    Left _ -> Nothing
    Right sv -> Map.lookup sv (vdKnownVulnerableVersion vd)

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
    withAnnotation (Log4jVulnerableReportItem name version origin vuln) =
      colorCoded $
        Text.intercalate
          " "
          [ name
          , formattedVersion
          , formattedVulnerability
          , toText . show $ origin
          ]
      where
        indeterminate = "indeterminate"
        colorCoded doc = if isNothing vuln then (pretty doc) else (annotateWarn $ pretty doc)
        formattedVersion = maybe indeterminate ("v" <>) (verConstraintToRevision =<< version)
        formattedVulnerability =
          if isNothing vuln
            then if formattedVersion == indeterminate then inBracket indeterminate else inBracket "Safe"
            else inBracket (toText . show $ vuln)

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
