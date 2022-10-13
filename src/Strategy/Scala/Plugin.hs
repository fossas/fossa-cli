{-# LANGUAGE TemplateHaskell #-}

module Strategy.Scala.Plugin (
  hasDependencyPlugins,
  genTreeJson,
) where

import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Stack (context)
import Data.Maybe (mapMaybe)
import Data.String.Conversion (ConvertUtf8 (decodeUtf8), toString)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextLazy
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  Has,
  execThrow,
 )
import Path (Abs, Dir, File, Path, mkRelFile, parent, parseAbsFile, (</>))

-- | Returns list of plugins used by sbt.
-- Ref: https://www.scala-sbt.org/1.x/docs/Plugins.html
getPlugins :: Command
getPlugins =
  Command
    { cmdName = "sbt"
    , cmdArgs =
        [ "--no-colors" -- to disable ANSI escape codes
        , "--batch" -- to disable interactivity
        , "plugins"
        ]
    , cmdAllowErr = Never
    }

-- | Returns list of plugins used by sbt.
hasDependencyPlugins :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Bool, Bool)
hasDependencyPlugins projectDir = do
  stdoutText <- (TextLazy.toStrict . decodeUtf8) <$> context "Identifying plugins" (execThrow projectDir getPlugins)
  pure
    ( Text.count "MiniDependencyTreePlugin" stdoutText > 0
    , Text.count "DependencyTreePlugin" stdoutText > 0
    )

-- | Generates Dependency Trees.
-- Ref: https://github.com/sbt/sbt/blob/master/main/src/main/scala/sbt/plugins/DependencyTreeSettings.scala#L101
--
-- This command unlike 'dependencyBrowseTree', does not open
-- the browser when executed.
--
-- It writes following files in target directory:
--  ./tree.json
--  ./tree.html
--  ./tree.data.js
mkDependencyBrowseTreeHTMLCmd :: Command
mkDependencyBrowseTreeHTMLCmd =
  Command
    { cmdName = "sbt"
    , cmdArgs =
        [ "--no-colors" -- to disable ANSI escape codes
        , "--batch" -- to disable interactivity
        , "dependencyBrowseTreeHTML"
        ]
    , cmdAllowErr = Never
    }

genTreeJson :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m [Path Abs File]
genTreeJson projectDir = do
  stdoutBL <- context "Generating dependency tree html" $ execThrow projectDir mkDependencyBrowseTreeHTMLCmd

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
