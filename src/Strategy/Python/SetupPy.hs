module Strategy.Python.SetupPy
  ( discover
  , analyze
  )
  where

import Prologue

import Control.Effect.Diagnostics
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Discovery.Walk
import Effect.ReadFS
import Strategy.Python.Util
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "setup.py") files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "python-setuppy" PythonGroup $
      analyze file

  pure WalkContinue

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsParser installRequiresParser file

mkProjectClosure :: Path Abs File -> [Req] -> ProjectClosureBody
mkProjectClosure file reqs = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph reqs
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

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
