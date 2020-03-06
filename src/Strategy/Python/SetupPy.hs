module Strategy.Python.SetupPy
  ( discover
  , analyze
  )
  where

import Prologue

import Control.Carrier.Error.Either
import Text.Megaparsec
import Text.Megaparsec.Char

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

  walkContinue

analyze :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Rel File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsParser installRequiresParser file

mkProjectClosure :: Path Rel File -> [Req] -> ProjectClosureBody
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
  prefix  = skipManyTill anySingle (string "install_requires=[")
  entries = between quote quote requirementParser `sepBy` char ','
  end     = char ']'

  quote   = char '\''
