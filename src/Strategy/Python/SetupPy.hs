module Strategy.Python.SetupPy
  ( analyze'
  )
  where

import Control.Effect.Diagnostics
import Data.Text (Text)
import Data.Void (Void)
import Effect.ReadFS
import Graphing (Graphing)
import Path
import Strategy.Python.Util
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = do
  reqs <- readContentsParser installRequiresParser file
  context "Building dependency graph" $ pure (buildGraph reqs)

type Parser = Parsec Void Text

installRequiresParser :: Parser [Req]
installRequiresParser = prefix *> entries <* end
  where
  prefix  = skipManyTill anySingle (symbol "install_requires") *> symbol "=" *> symbol "["
  entries = (requireSurroundedBy "\"" <|> requireSurroundedBy "\'") `sepEndBy` symbol ","

  requireSurroundedBy :: Text -> Parser Req
  requireSurroundedBy quote = between (symbol quote) (symbol quote) requirementParser

  end     = symbol "]"

  symbol :: Text -> Parser Text
  symbol = L.symbol space
