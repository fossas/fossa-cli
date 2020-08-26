module Strategy.Python.ReqTxt
  ( discover
  , analyze
  )
  where

import Control.Effect.Diagnostics
import Control.Monad (unless, when)
import Data.Foldable (asum)
import Data.List (isInfixOf, isSuffixOf)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Void (Void)
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import Path
import Strategy.Python.Util
import Text.Megaparsec
import Text.Megaparsec.Char
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \dir _ files -> do
  let txtFiles = filter (\f -> "req" `isInfixOf` fileName f
                            && ".txt" `isSuffixOf` fileName f) files

  unless (null txtFiles) $
    runSimpleStrategy "python-requirements" PythonGroup $ analyze dir txtFiles

  pure WalkContinue

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> [Path Abs File] -> m ProjectClosureBody
analyze projectDir files = do
  results <- traverse (recover . analyzeSingle) files
  let successful = catMaybes results

  when (null successful) $ fatalText "Analysis failed for all discovered *req*.txt files"

  let graphing :: Graphing Dependency
      graphing = mconcat successful

  pure (mkProjectClosure projectDir graphing)

analyzeSingle :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyzeSingle file = buildGraph <$> readContentsParser requirementsTxtParser file

mkProjectClosure :: Path Abs Dir -> Graphing Dependency -> ProjectClosureBody
mkProjectClosure dir graph = ProjectClosureBody
  { bodyModuleDir    = dir
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = graph
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

type Parser = Parsec Void Text

-- https://pip.pypa.io/en/stable/reference/pip_install/#requirements-file-format
requirementsTxtParser :: Parser [Req]
requirementsTxtParser = concat <$> ((line `sepBy` eol) <* eof)
  where
  isEndLine :: Char -> Bool
  isEndLine '\n' = True
  isEndLine '\r' = True
  isEndLine _    = False

  -- ignore content until the end of the line
  ignored :: Parser ()
  ignored = () <$ takeWhileP (Just "ignored") (not . isEndLine)

  comment :: Parser ()
  comment = char '#' *> ignored

  -- FUTURE: we can case split / sum-type this for better analysis
  line = [] <$ char '-' <* ignored -- pip options
     <|> [] <$ char '.' <* ignored -- relative path
     <|> [] <$ char '/' <* ignored -- absolute path
     <|> [] <$ oneOfS ["http:", "https:", "git+", "hg+", "svn+", "bzr+"] <* ignored -- URLs
     <|> [] <$ comment
     <|> (pure <$> requirementParser <* optional comment)
     <|> pure [] -- empty line

  oneOfS = asum . map string
