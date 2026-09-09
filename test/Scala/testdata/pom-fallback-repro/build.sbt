// Minimal repro for the Scala sbt-generated-pom fallback bug (Strategy.Scala.analyzeWithPoms).
//
// disablePlugins(MiniDependencyTreePlugin) forces fossa onto the pom fallback:
// with no dependency-tree task available, getDeps() falls through
// analyzeWithDepTreeJson/analyzeWithSbtDepTree (which DO shrink roots) to
// analyzeWithPoms, which returned Pom.analyze' un-shrunk -- reporting the
// project's own artifact as the sole Direct dependency.
//
// Buggy fallback output: Imports = ["mvn+com.example:pom-fallback-repro_2.13$0.1.0"]
// Expected  (post-fix):  Imports = ["mvn+org.scala-lang:scala-library$2.13.14",
//                                   "mvn+org.typelevel:cats-core_2.13$2.10.0"]
// (the sbt-generated pom declares both at top level; the sbt-native tactics
// reach the same cats-core via full resolution with scala-library as its
// transitive)

ThisBuild / organization := "com.example"
ThisBuild / version := "0.1.0"

name := "pom-fallback-repro"
scalaVersion := "2.13.14"

disablePlugins(MiniDependencyTreePlugin)

libraryDependencies += "org.typelevel" % "cats-core_2.13" % "2.10.0"
