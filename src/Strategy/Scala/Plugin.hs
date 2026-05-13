{-# LANGUAGE TemplateHaskell #-}

module Strategy.Scala.Plugin (
  hasDependencyPlugins,
  detectDependencyPlugins,
  genTreeJson,
  DependencyTreePluginKind (..),
  DependencyPluginsDetected (..),
) where

import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Stack (context)
import Data.Maybe (mapMaybe)
import Data.String.Conversion (ConvertUtf8 (decodeUtf8), toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextLazy
import Effect.Exec (
  Command (..),
  Exec,
  Has,
  execThrow,
 )
import Path (Abs, Dir, File, Path, mkRelFile, parent, parseAbsFile, (</>))
import Strategy.Scala.Common (mkSbtCommand)

-- | Which non-mini dependency-tree plugin (if any) the project has installed.
--
-- The two plugins differ in the casing of their @dependencyBrowseTree@ task.
-- See 'mkDependencyBrowseTreeCmd' for the command names.
data DependencyTreePluginKind
  = -- | @sbt.plugins.DependencyTreePlugin@. Built into sbt 1.4+ and enabled
    -- explicitly via @addDependencyTreePlugin@ in @plugins.sbt@. Provides the
    -- uppercase @dependencyBrowseTreeHTML@ task.
    ModernDependencyTreePlugin
  | -- | @net.virtualvoid.sbt.graph.DependencyGraphPlugin@. The third-party
    -- @sbt-dependency-graph@ plugin used on sbt < 1.4. Provides the lowercase
    -- @dependencyBrowseTreeHtml@ task.
    LegacyDependencyGraphPlugin
  deriving (Eq, Ord, Show)

-- | What the @sbt plugins@ output told us about dependency-tree plugins.
data DependencyPluginsDetected = DependencyPluginsDetected
  { hasMiniDependencyTreePlugin :: Bool
  , dependencyTreePlugin :: Maybe DependencyTreePluginKind
  }
  deriving (Eq, Ord, Show)

-- | Returns list of plugins used by sbt.
-- Ref: https://www.scala-sbt.org/1.x/docs/Plugins.html
getPlugins :: Command
getPlugins = mkSbtCommand "plugins"

-- | Detect which dependency-tree plugins are loaded by running @sbt plugins@.
hasDependencyPlugins :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m DependencyPluginsDetected
hasDependencyPlugins projectDir = do
  stdoutText <- (TextLazy.toStrict . decodeUtf8) <$> context "Identifying plugins" (execThrow projectDir getPlugins)
  pure $ detectDependencyPlugins stdoutText

-- | Classify dependency-tree plugins from the @sbt plugins@ output.
--
-- The plugin names mapped here:
--
--    * @sbt.plugins.MiniDependencyTreePlugin@ — bundled with sbt 1.4+, gives
--      us the @dependencyTree@ task used by 'Strategy.Scala.SbtDependencyTree'.
--    * @sbt.plugins.DependencyTreePlugin@ — opt-in on sbt 1.4+ via
--      @addDependencyTreePlugin@. Provides the uppercase
--      @dependencyBrowseTreeHTML@ task.
--    * @net.virtualvoid.sbt.graph.DependencyGraphPlugin@ — third-party plugin
--      used on sbt < 1.4. Provides the lowercase @dependencyBrowseTreeHtml@
--      task.
--
-- Detection anchors on the @\<FQCN\>: enabled in@ suffix rather than the bare
-- FQCN. @sbt plugins@ lists plugins the user has explicitly disabled (via
-- @disablePlugins(...)@) with @: disabled in \<scope\>@ — those still
-- contain the FQCN as a substring, so an unanchored match would wrongly
-- route to a task that doesn't exist on the active plugin set.
--
-- When both modern and legacy non-mini plugins are present we prefer the
-- modern one (sbt 1.4+ wins) since legacy plugin presence on a modern sbt
-- typically means the user has both kinds of declarations in their build.
detectDependencyPlugins :: Text -> DependencyPluginsDetected
detectDependencyPlugins stdoutText =
  DependencyPluginsDetected
    { hasMiniDependencyTreePlugin = "sbt.plugins.MiniDependencyTreePlugin: enabled in" `Text.isInfixOf` stdoutText
    , dependencyTreePlugin =
        if "sbt.plugins.DependencyTreePlugin: enabled in" `Text.isInfixOf` stdoutText
          then Just ModernDependencyTreePlugin
          else
            if "net.virtualvoid.sbt.graph.DependencyGraphPlugin: enabled in" `Text.isInfixOf` stdoutText
              then Just LegacyDependencyGraphPlugin
              else Nothing
    }

-- | The sbt task that writes @tree.html@/@tree.json@ alongside its dependency
-- output. Plugin name vs task casing:
--
--    * 'ModernDependencyTreePlugin' (sbt 1.4+, @addDependencyTreePlugin@) →
--      @dependencyBrowseTreeHTML@.
--    * 'LegacyDependencyGraphPlugin' (sbt < 1.4, @sbt-dependency-graph@) →
--      @dependencyBrowseTreeHtml@.
--
-- Picking the wrong casing produces an sbt error like
-- @[error] Not a valid command: dependencyBrowseTreeHTML@ which surfaces to
-- the user as "Could not retrieve output from sbt dependencyBrowseTreeHTML".
-- That regression (CLI 3.8.30) is tracked under TKT-15490 / ANE-2718.
mkDependencyBrowseTreeCmd :: DependencyTreePluginKind -> Command
mkDependencyBrowseTreeCmd ModernDependencyTreePlugin = mkSbtCommand "dependencyBrowseTreeHTML"
mkDependencyBrowseTreeCmd LegacyDependencyGraphPlugin = mkSbtCommand "dependencyBrowseTreeHtml"

-- | Generates dependency trees by invoking the appropriate
-- @dependencyBrowseTree*@ task. Unlike @dependencyBrowseTree@, this does not
-- open a browser when executed.
--
-- It writes the following files in the target directory:
--
--    * @./tree.json@
--    * @./tree.html@
--    * @./tree.data.js@
genTreeJson :: (Has Exec sig m, Has Diagnostics sig m) => DependencyTreePluginKind -> Path Abs Dir -> m [Path Abs File]
genTreeJson pluginKind projectDir = do
  stdoutBL <- context "Generating dependency tree html" $ execThrow projectDir (mkDependencyBrowseTreeCmd pluginKind)

  -- stdout for "sbt dependencyBrowseTreeHTML" looks like:
  -- -
  -- > ...
  -- > [info] loading settings for project global from build.sbt ...
  -- > [info] ...
  -- > [info] HTML tree written to file:/absolute/path/to/tree.html
  -- > [info] HTML tree written to file:/absolute/path/to/other/tree.html
  -- > ...
  let stdoutLText = decodeUtf8 stdoutBL
      stdout = TextLazy.toStrict stdoutLText
      --
      stdoutLines :: [Text.Text]
      stdoutLines = Text.lines stdout
      --
      htmlLines :: [Text.Text]
      htmlLines = mapMaybe (Text.stripPrefix "[info] HTML tree written to file:") stdoutLines
      --
      htmlLocation :: Maybe [Path Abs File]
      htmlLocation = traverse (parseAbsFile . toString) htmlLines
      --
      -- ref: https://github.com/sbt/sbt/blob/master/main/src/main/scala/sbt/internal/graph/rendering/TreeView.scala#L33
      treeLocation :: Maybe [Path Abs File] -> Maybe [Path Abs File]
      treeLocation (Nothing) = Nothing
      treeLocation (Just poms) = Just $ map (\p -> parent p </> $(mkRelFile "tree.json")) poms

  case treeLocation htmlLocation of
    Nothing -> fatalText ("Could not identify tree json paths from:\n" <> Text.unlines htmlLines)
    Just [] -> fatalText "No sbt projects found"
    Just paths -> pure paths
