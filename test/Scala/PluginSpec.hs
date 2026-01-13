{-# LANGUAGE QuasiQuotes #-}

module Scala.PluginSpec (
  spec,
) where

import Data.Text (Text)
import Strategy.Scala.Plugin (detectDependencyPlugins)
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
      detectDependencyPlugins sbt14BuiltinOnly `shouldBe` (True, False)

    it "should detect explicit DependencyTreePlugin" $ do
      detectDependencyPlugins sbtExplicitPluginOnly `shouldBe` (False, True)

    it "should detect legacy net.virtualvoid plugin" $ do
      detectDependencyPlugins sbtLegacyVirtualvoidPlugin `shouldBe` (False, True)

    -- TKT-14742: When both plugins present, findProjects should prefer MiniDependencyTreePlugin
    it "should detect both plugins when MiniDependencyTreePlugin AND explicit plugin present" $ do
      detectDependencyPlugins sbt14WithExplicitPlugin `shouldBe` (True, True)

    it "should detect no plugins when neither is present" $ do
      detectDependencyPlugins sbtNoPlugins `shouldBe` (False, False)

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

-- sbt < 1.4 with explicit addDependencyTreePlugin
sbtExplicitPluginOnly :: Text
sbtExplicitPluginOnly =
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
