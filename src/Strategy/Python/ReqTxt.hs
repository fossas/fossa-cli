module Strategy.Python.ReqTxt (
  analyze',
  requirementsTxtParser,
) where

import Control.Effect.Diagnostics
import Control.Monad (void)
import Data.Foldable (asum)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Effect.ReadFS
import Errata (Errata (..))
import Graphing (Graphing)
import Path
import Strategy.Python.Pip (PythonPackage)
import Strategy.Python.Util
import Text.Megaparsec
import Text.Megaparsec.Char
import Types

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Maybe [PythonPackage] -> Path Abs File -> m (Graphing Dependency)
analyze' packages file = do
  reqs <- errCtx (ReqsTxtFailedCtx file) $ errHelp ReqsTxtFailedHelp $ readContentsParser requirementsTxtParser file
  context "Building dependency graph" $ pure (buildGraph packages reqs)

data ReqsTxtFailed
  = ReqsTxtFailedCtx (Path Abs File)
  | ReqsTxtFailedHelp
instance ToDiagnostic ReqsTxtFailed where
  renderDiagnostic (ReqsTxtFailedCtx path) = do
    let header = "Failed to parse: " <> toText (show path)
    Errata (Just header) [] Nothing
  renderDiagnostic ReqsTxtFailedHelp = do
    let header = "Ignore this error if this file isn't a python requirements.txt file."
    Errata (Just header) [] Nothing

type Parser = Parsec Void Text

-- https://pip.pypa.io/en/stable/reference/requirements-file-format/
requirementsTxtParser :: Parser [Req]
requirementsTxtParser = concat <$> manyTill reqParser eof

reqParser :: Parser [Req]
reqParser =
  [] <$ char '-' <* ignored -- pip options
    <|> [] <$ char '.' <* ignored -- relative path
    <|> [] <$ char '/' <* ignored -- absolute path
    <|> [] <$ oneOfS ["http:", "https:", "git+", "hg+", "svn+", "bzr+"] <* ignored -- URLs
    <|> [] <$ comment
    <|> (pure <$> try requirementParser <* ignored)
    <|> [] <$ ignored -- something else we're not able to parse
  where
    isEndLine :: Char -> Bool
    isEndLine '\n' = True
    isEndLine '\r' = True
    isEndLine _ = False

    -- ignore content until the end of the line
    ignored :: Parser ()
    ignored = void $ takeWhileP (Just "ignored") (not . isEndLine) <* takeWhileP (Just "end of line") isEndLine

    comment :: Parser ()
    comment = char '#' *> ignored

    oneOfS = asum . map string
