{-# LANGUAGE QuasiQuotes #-}

module Scala.PluginSpec (
  spec,
) where

import Data.Text (Text)
import Strategy.Scala.Plugin (
  DependencyPluginsDetected (..),
  DependencyTreePluginKind (..),
  detectDependencyPlugins,
 )
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )
import Text.RawString.QQ (r)

-- The fixtures below mirror the real layout of @sbt -batch -no-colors plugins@:
-- per-project "Enabled plugins in <project>:" sections list one bare plugin
-- FQCN per indented line, and a trailing "Plugins that are loaded to the build
-- but not enabled in any subprojects:" section lists the rest. There is no
-- "<FQCN>: enabled in <scope>" suffix in real output. The sbt 1.9.8 fixtures
-- (default, addDependencyTreePlugin, disabled-mini) are verbatim captures from
-- fossas/scala3-example-project; the remainder follow the same layout.
spec :: Spec
spec = do
  describe "detectDependencyPlugins" $ do
    -- Regression guard for TKT-15490: this is the verbatim `sbt plugins`
    -- output for fossas/scala3-example-project (sbt 1.9.8, no plugins.sbt).
    -- The built-in MiniDependencyTreePlugin must be detected so the deep
    -- `dependencyTree` path runs. A prior refactor anchored detection on a
    -- non-existent "<FQCN>: enabled in" suffix, returned False here, and
    -- silently fell back to generated poms (Partial graph).
    it "detects the built-in MiniDependencyTreePlugin (sbt 1.9.8, no plugins.sbt)" $ do
      detectDependencyPlugins sbt198DefaultBuiltin
        `shouldBe` DependencyPluginsDetected{hasMiniDependencyTreePlugin = True, dependencyTreePlugin = Nothing}

    -- Real sbt 1.9.8 output with `addDependencyTreePlugin` in project/plugins.sbt
    -- (the customer setup from TKT-15490). sbt enables the explicit
    -- DependencyTreePlugin alongside the built-in MiniDependencyTreePlugin.
    it "detects MiniDependencyTreePlugin and modern DependencyTreePlugin together (addDependencyTreePlugin)" $ do
      detectDependencyPlugins sbt198AddDependencyTreePlugin
        `shouldBe` DependencyPluginsDetected{hasMiniDependencyTreePlugin = True, dependencyTreePlugin = Just ModernDependencyTreePlugin}

    -- sbt < 1.4 with the third-party net.virtualvoid sbt-dependency-graph
    -- plugin. MiniDependencyTreePlugin does not exist before sbt 1.4, so it is
    -- absent; routing must dispatch to the legacy lowercase task.
    it "detects the legacy net.virtualvoid plugin (sbt-dependency-graph)" $ do
      detectDependencyPlugins sbtLegacyVirtualvoidPlugin
        `shouldBe` DependencyPluginsDetected{hasMiniDependencyTreePlugin = False, dependencyTreePlugin = Just LegacyDependencyGraphPlugin}

    -- TKT-15490 routing guard: when the modern DependencyTreePlugin is enabled
    -- but MiniDependencyTreePlugin is not, findProjects routes to genTreeJson,
    -- which must select the uppercase `dependencyBrowseTreeHTML` task. The
    -- classification must be Modern (not Legacy lowercase).
    it "classifies a modern DependencyTreePlugin with no MiniDependencyTreePlugin as Modern" $ do
      detectDependencyPlugins sbtModernWithoutMini
        `shouldBe` DependencyPluginsDetected{hasMiniDependencyTreePlugin = False, dependencyTreePlugin = Just ModernDependencyTreePlugin}

    -- If a build somehow enables both the modern and legacy plugin, prefer the
    -- modern one — on sbt 1.4+ the legacy lowercase task is unavailable.
    it "prefers modern DependencyTreePlugin when both modern and legacy are enabled" $ do
      detectDependencyPlugins sbtBothModernAndLegacy
        `shouldBe` DependencyPluginsDetected{hasMiniDependencyTreePlugin = False, dependencyTreePlugin = Just ModernDependencyTreePlugin}

    it "detects no dependency-tree plugins when none are enabled" $ do
      detectDependencyPlugins sbtNoPlugins
        `shouldBe` DependencyPluginsDetected{hasMiniDependencyTreePlugin = False, dependencyTreePlugin = Nothing}

    -- `disablePlugins(MiniDependencyTreePlugin)` moves the plugin into the
    -- trailing "loaded to the build but not enabled in any subprojects"
    -- section. This is a verbatim sbt 1.9.8 capture; detection must not treat
    -- it as active. An unanchored substring match (the pre-refactor behavior)
    -- would wrongly count it.
    it "ignores a MiniDependencyTreePlugin listed as loaded-but-not-enabled" $ do
      detectDependencyPlugins sbt198DisabledMini
        `shouldBe` DependencyPluginsDetected{hasMiniDependencyTreePlugin = False, dependencyTreePlugin = Nothing}

    it "ignores a modern DependencyTreePlugin listed as loaded-but-not-enabled" $ do
      detectDependencyPlugins sbtDisabledModern
        `shouldBe` DependencyPluginsDetected{hasMiniDependencyTreePlugin = False, dependencyTreePlugin = Nothing}

-- Verbatim `sbt -batch -no-colors plugins` output for fossas/scala3-example-project
-- (sbt 1.9.8, no project/plugins.sbt).
sbt198DefaultBuiltin :: Text
sbt198DefaultBuiltin =
  [r|[info] welcome to sbt 1.9.8 (Homebrew Java 24.0.1)
[info] loading project definition from /private/tmp/scala3-example-project/project
[info] loading settings for project root from build.sbt ...
[info] set current project to scala3-example-project (in build file:/private/tmp/scala3-example-project/)
In build /private/tmp/scala3-example-project/:
  Enabled plugins in root:
    sbt.plugins.CorePlugin
    sbt.plugins.Giter8TemplatePlugin
    sbt.plugins.IvyPlugin
    sbt.plugins.JUnitXmlReportPlugin
    sbt.plugins.JvmPlugin
    sbt.plugins.MiniDependencyTreePlugin
    sbt.plugins.SemanticdbPlugin
Plugins that are loaded to the build but not enabled in any subprojects:
  sbt.ScriptedPlugin
  sbt.plugins.SbtPlugin
|]

-- Verbatim sbt 1.9.8 output with `addDependencyTreePlugin` added to
-- project/plugins.sbt (the TKT-15490 customer setup).
sbt198AddDependencyTreePlugin :: Text
sbt198AddDependencyTreePlugin =
  [r|[info] welcome to sbt 1.9.8 (Homebrew Java 24.0.1)
[info] loading project definition from /private/tmp/scala3-example-project/project
[info] loading settings for project root from build.sbt ...
[info] set current project to scala3-example-project (in build file:/private/tmp/scala3-example-project/)
In build /private/tmp/scala3-example-project/:
  Enabled plugins in root:
    sbt.plugins.CorePlugin
    sbt.plugins.DependencyTreePlugin
    sbt.plugins.Giter8TemplatePlugin
    sbt.plugins.IvyPlugin
    sbt.plugins.JUnitXmlReportPlugin
    sbt.plugins.JvmPlugin
    sbt.plugins.MiniDependencyTreePlugin
    sbt.plugins.SemanticdbPlugin
Plugins that are loaded to the build but not enabled in any subprojects:
  sbt.ScriptedPlugin
  sbt.plugins.SbtPlugin
|]

-- sbt < 1.4 with the legacy net.virtualvoid sbt-dependency-graph plugin.
sbtLegacyVirtualvoidPlugin :: Text
sbtLegacyVirtualvoidPlugin =
  [r|[info] welcome to sbt 1.2.8 (Eclipse Adoptium Java 11.0.21)
[info] set current project to test-project (in build file:/Users/test/project/)
In build /Users/test/project/:
  Enabled plugins in root:
    sbt.plugins.CorePlugin
    sbt.plugins.IvyPlugin
    sbt.plugins.JvmPlugin
    net.virtualvoid.sbt.graph.DependencyGraphPlugin
Plugins that are loaded to the build but not enabled in any subprojects:
  sbt.plugins.SbtPlugin
|]

-- Modern DependencyTreePlugin enabled with MiniDependencyTreePlugin disabled
-- (`disablePlugins(MiniDependencyTreePlugin)` alongside addDependencyTreePlugin).
sbtModernWithoutMini :: Text
sbtModernWithoutMini =
  [r|[info] welcome to sbt 1.11.5 (Eclipse Adoptium Java 17.0.10)
[info] set current project to test-project (in build file:/Users/test/project/)
In build /Users/test/project/:
  Enabled plugins in root:
    sbt.plugins.CorePlugin
    sbt.plugins.DependencyTreePlugin
    sbt.plugins.IvyPlugin
    sbt.plugins.JvmPlugin
Plugins that are loaded to the build but not enabled in any subprojects:
  sbt.plugins.MiniDependencyTreePlugin
|]

-- A pathological build that enables both the modern and the legacy plugin.
sbtBothModernAndLegacy :: Text
sbtBothModernAndLegacy =
  [r|[info] welcome to sbt 1.9.7 (Eclipse Adoptium Java 11.0.21)
[info] set current project to test-project (in build file:/Users/test/project/)
In build /Users/test/project/:
  Enabled plugins in root:
    sbt.plugins.CorePlugin
    sbt.plugins.DependencyTreePlugin
    sbt.plugins.IvyPlugin
    sbt.plugins.JvmPlugin
    net.virtualvoid.sbt.graph.DependencyGraphPlugin
Plugins that are loaded to the build but not enabled in any subprojects:
  sbt.plugins.SbtPlugin
|]

sbtNoPlugins :: Text
sbtNoPlugins =
  [r|[info] welcome to sbt 1.9.7 (Eclipse Adoptium Java 11.0.21)
[info] set current project to test-project (in build file:/Users/test/project/)
In build /Users/test/project/:
  Enabled plugins in root:
    sbt.plugins.CorePlugin
    sbt.plugins.IvyPlugin
    sbt.plugins.JvmPlugin
Plugins that are loaded to the build but not enabled in any subprojects:
  sbt.plugins.SbtPlugin
|]

-- Verbatim sbt 1.9.8 output with `disablePlugins(sbt.plugins.MiniDependencyTreePlugin)`
-- on the root project. The plugin appears only in the trailing not-enabled
-- section.
sbt198DisabledMini :: Text
sbt198DisabledMini =
  [r|[info] welcome to sbt 1.9.8 (Homebrew Java 24.0.1)
[info] loading project definition from /private/tmp/scala3-example-project/project
[info] loading settings for project root from build.sbt ...
[info] set current project to scala3-example-project (in build file:/private/tmp/scala3-example-project/)
In build /private/tmp/scala3-example-project/:
  Enabled plugins in root:
    sbt.plugins.CorePlugin
    sbt.plugins.Giter8TemplatePlugin
    sbt.plugins.IvyPlugin
    sbt.plugins.JUnitXmlReportPlugin
    sbt.plugins.JvmPlugin
    sbt.plugins.SemanticdbPlugin
Plugins that are loaded to the build but not enabled in any subprojects:
  sbt.ScriptedPlugin
  sbt.plugins.MiniDependencyTreePlugin
  sbt.plugins.SbtPlugin
|]

-- Modern DependencyTreePlugin disabled via disablePlugins; it appears only in
-- the trailing not-enabled section, so routing must not target its task.
sbtDisabledModern :: Text
sbtDisabledModern =
  [r|[info] welcome to sbt 1.11.5 (Eclipse Adoptium Java 17.0.10)
[info] set current project to test-project (in build file:/Users/test/project/)
In build /Users/test/project/:
  Enabled plugins in root:
    sbt.plugins.CorePlugin
    sbt.plugins.IvyPlugin
    sbt.plugins.JvmPlugin
Plugins that are loaded to the build but not enabled in any subprojects:
  sbt.plugins.DependencyTreePlugin
|]
