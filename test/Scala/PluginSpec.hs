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

spec :: Spec
spec = do
  describe "detectDependencyPlugins" $ do
    it "should detect MiniDependencyTreePlugin (sbt 1.4+ built-in)" $ do
      detectDependencyPlugins sbt14BuiltinOnly
        `shouldBe` DependencyPluginsDetected{hasMiniDependencyTreePlugin = True, dependencyTreePlugin = Nothing}

    it "should detect explicit modern DependencyTreePlugin (sbt 1.4+ addDependencyTreePlugin)" $ do
      detectDependencyPlugins sbtModernExplicitPluginOnly
        `shouldBe` DependencyPluginsDetected{hasMiniDependencyTreePlugin = False, dependencyTreePlugin = Just ModernDependencyTreePlugin}

    it "should detect legacy net.virtualvoid plugin (sbt < 1.4 sbt-dependency-graph)" $ do
      detectDependencyPlugins sbtLegacyVirtualvoidPlugin
        `shouldBe` DependencyPluginsDetected{hasMiniDependencyTreePlugin = False, dependencyTreePlugin = Just LegacyDependencyGraphPlugin}

    -- TKT-14742: When both plugins present, findProjects should prefer MiniDependencyTreePlugin
    it "should detect both plugins when MiniDependencyTreePlugin AND modern explicit plugin present" $ do
      detectDependencyPlugins sbt14WithExplicitPlugin
        `shouldBe` DependencyPluginsDetected{hasMiniDependencyTreePlugin = True, dependencyTreePlugin = Just ModernDependencyTreePlugin}

    -- TKT-15490: sbt 1.11.5 with addDependencyTreePlugin and no auto-loaded
    -- MiniDependencyTreePlugin must be classified as ModernDependencyTreePlugin
    -- so the analyzer runs the uppercase `dependencyBrowseTreeHTML` task. The
    -- pre-fix code returned (False, True) and the routing dispatched to the
    -- legacy lowercase `dependencyBrowseTreeHtml`, which sbt 1.4+ rejects.
    it "should classify modern DependencyTreePlugin alone as Modern (TKT-15490 routing guard)" $ do
      let detected = detectDependencyPlugins sbt111ExplicitPluginOnly
      hasMiniDependencyTreePlugin detected `shouldBe` False
      dependencyTreePlugin detected `shouldBe` Just ModernDependencyTreePlugin

    -- If a project somehow lists both the modern and legacy plugin, prefer
    -- the modern one — sbt 1.4+ wins, since the legacy plugin will not
    -- function on a sbt that also surfaces sbt.plugins.DependencyTreePlugin.
    it "should prefer modern DependencyTreePlugin when both modern and legacy are present" $ do
      detectDependencyPlugins sbtBothModernAndLegacy
        `shouldBe` DependencyPluginsDetected{hasMiniDependencyTreePlugin = False, dependencyTreePlugin = Just ModernDependencyTreePlugin}

    it "should detect no plugins when neither is present" $ do
      detectDependencyPlugins sbtNoPlugins
        `shouldBe` DependencyPluginsDetected{hasMiniDependencyTreePlugin = False, dependencyTreePlugin = Nothing}

-- sbt 1.4+ with only built-in plugin
sbt14BuiltinOnly :: Text
sbt14BuiltinOnly =
  [r|[info] welcome to sbt 1.9.7 (Eclipse Adoptium Java 11.0.21)
[info] loading global plugins from /Users/test/.sbt/1.0/plugins
[info] loading project definition from /Users/test/project/project
[info] loading settings for project root from build.sbt ...
[info] set current project to test-project (in build file:/Users/test/project/)
[info] In file:/Users/test/project/
[info]   sbt.plugins.CorePlugin: enabled in root
[info]   sbt.plugins.IvyPlugin: enabled in root
[info]   sbt.plugins.JvmPlugin: enabled in root
[info]   sbt.plugins.MiniDependencyTreePlugin: enabled in root
[info]   sbt.plugins.SemanticdbPlugin: enabled in root
|]

-- sbt 1.4+ with explicit addDependencyTreePlugin and no MiniDependencyTreePlugin
-- listed (the case the customer in TKT-15490 hit on sbt 1.11.5).
sbtModernExplicitPluginOnly :: Text
sbtModernExplicitPluginOnly =
  [r|[info] welcome to sbt 1.3.13 (Eclipse Adoptium Java 11.0.21)
[info] loading global plugins from /Users/test/.sbt/1.0/plugins
[info] loading project definition from /Users/test/project/project
[info] loading settings for project root from build.sbt ...
[info] set current project to test-project (in build file:/Users/test/project/)
[info] In file:/Users/test/project/
[info]   sbt.plugins.CorePlugin: enabled in root
[info]   sbt.plugins.IvyPlugin: enabled in root
[info]   sbt.plugins.JvmPlugin: enabled in root
[info]   sbt.plugins.DependencyTreePlugin: enabled in root
|]

-- sbt 1.11.5 with addDependencyTreePlugin in plugins.sbt — mirrors the
-- customer environment from TKT-15490. The customer reported that the
-- pre-fix CLI invoked the lowercase `dependencyBrowseTreeHtml`, which sbt
-- 1.4+ rejects.
sbt111ExplicitPluginOnly :: Text
sbt111ExplicitPluginOnly =
  [r|[info] welcome to sbt 1.11.5 (Eclipse Adoptium Java 17.0.10)
[info] loading global plugins from /Users/test/.sbt/1.0/plugins
[info] loading project definition from /Users/test/project/project
[info] loading settings for project root from build.sbt ...
[info] set current project to test-project (in build file:/Users/test/project/)
[info] In file:/Users/test/project/
[info]   sbt.plugins.CorePlugin: enabled in root
[info]   sbt.plugins.IvyPlugin: enabled in root
[info]   sbt.plugins.JvmPlugin: enabled in root
[info]   sbt.plugins.DependencyTreePlugin: enabled in root
|]

-- sbt < 1.4 with legacy net.virtualvoid plugin
sbtLegacyVirtualvoidPlugin :: Text
sbtLegacyVirtualvoidPlugin =
  [r|[info] welcome to sbt 1.2.8 (Eclipse Adoptium Java 11.0.21)
[info] loading global plugins from /Users/test/.sbt/1.0/plugins
[info] loading project definition from /Users/test/project/project
[info] loading settings for project root from build.sbt ...
[info] set current project to test-project (in build file:/Users/test/project/)
[info] In file:/Users/test/project/
[info]   sbt.plugins.CorePlugin: enabled in root
[info]   sbt.plugins.IvyPlugin: enabled in root
[info]   sbt.plugins.JvmPlugin: enabled in root
[info]   net.virtualvoid.sbt.graph.DependencyGraphPlugin: enabled in root
|]

-- TKT-14742: sbt 1.4+ with BOTH built-in and explicit plugin
sbt14WithExplicitPlugin :: Text
sbt14WithExplicitPlugin =
  [r|[info] welcome to sbt 1.9.7 (Eclipse Adoptium Java 11.0.21)
[info] loading global plugins from /Users/test/.sbt/1.0/plugins
[info] loading project definition from /Users/test/project/project
[info] loading settings for project root from build.sbt ...
[info] set current project to test-project (in build file:/Users/test/project/)
[info] In file:/Users/test/project/
[info]   sbt.plugins.CorePlugin: enabled in root
[info]   sbt.plugins.IvyPlugin: enabled in root
[info]   sbt.plugins.JvmPlugin: enabled in root
[info]   sbt.plugins.MiniDependencyTreePlugin: enabled in root
[info]   sbt.plugins.DependencyTreePlugin: enabled in root
[info]   sbt.plugins.SemanticdbPlugin: enabled in root
|]

-- A pathological setup that lists both modern and legacy plugins.
sbtBothModernAndLegacy :: Text
sbtBothModernAndLegacy =
  [r|[info] welcome to sbt 1.9.7 (Eclipse Adoptium Java 11.0.21)
[info] In file:/Users/test/project/
[info]   sbt.plugins.CorePlugin: enabled in root
[info]   sbt.plugins.IvyPlugin: enabled in root
[info]   sbt.plugins.JvmPlugin: enabled in root
[info]   sbt.plugins.DependencyTreePlugin: enabled in root
[info]   net.virtualvoid.sbt.graph.DependencyGraphPlugin: enabled in root
|]

sbtNoPlugins :: Text
sbtNoPlugins =
  [r|[info] welcome to sbt 1.9.7 (Eclipse Adoptium Java 11.0.21)
[info] loading global plugins from /Users/test/.sbt/1.0/plugins
[info] loading project definition from /Users/test/project/project
[info] loading settings for project root from build.sbt ...
[info] set current project to test-project (in build file:/Users/test/project/)
[info] In file:/Users/test/project/
[info]   sbt.plugins.CorePlugin: enabled in root
[info]   sbt.plugins.IvyPlugin: enabled in root
[info]   sbt.plugins.JvmPlugin: enabled in root
|]
