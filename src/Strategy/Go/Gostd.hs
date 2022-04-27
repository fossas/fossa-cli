module Strategy.Go.Gostd (
  GoStdlibDep (..),
  listGoStdlibPackages,
  filterGoStdlibPackages,
  parseGoStdlibPackages,
  parseGoStdlibPackage,
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Data.Char (isSpace)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace (trace)
import DepTypes (DepType (GoType), Dependency (dependencyName))
import Effect.Exec (AllowErr (Never), Command (..), Exec, execParser)
import Graphing (Graphing, filter)
import Path (Abs, Dir, Path)
import Text.Megaparsec (MonadParsec (eof), Parsec, empty, many, takeWhile1P)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

data GoStdlibDep = GoStdlibDep
  { goStdlibDepType :: DepType
  , goStdlibDepName :: Text
  }
  deriving (Show, Eq)

mkDep :: Text -> GoStdlibDep
mkDep = GoStdlibDep GoType

goListStdCommand :: Command
goListStdCommand =
  Command
    { cmdName = "go"
    , cmdArgs = ["list", "std"]
    , cmdAllowErr = Never
    }

listGoStdlibPackages :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m [GoStdlibDep]
listGoStdlibPackages root = execParser parseGoStdlibPackages root goListStdCommand

parseGoStdlibPackages :: Parser [GoStdlibDep]
parseGoStdlibPackages = many parseGoStdlibPackage <* eof

parseGoStdlibPackage :: Parser GoStdlibDep
parseGoStdlibPackage = mkDep <$> ident

filterGoStdlibPackages :: [GoStdlibDep] -> Graphing Dependency -> Graphing Dependency
filterGoStdlibPackages stdlib = Graphing.filter $ not . shouldFilter
  where
    stdlib' :: Set Text
    stdlib' = Set.fromList $ fmap goStdlibDepName stdlib

    isStdLib :: Dependency -> Bool
    isStdLib = flip Set.member stdlib' . dependencyName

    isExtra :: Dependency -> Bool
    isExtra = flip Set.member extraPackageFilters . dependencyName

    shouldFilter :: Dependency -> Bool
    shouldFilter dep = isStdLib dep || isExtra dep

-- | These aren't part of the standard library, but are extra packages we also want to ignore.
-- Each package should have a comment explaining what it is.
extraPackageFilters :: Set Text
extraPackageFilters =
  Set.fromList
    [ "C" -- Not a real package - this is a magic import used by Go to declare C interop: https://go.dev/blog/cgo
    ]

-- | Consume spaces.
sc :: Parser ()
sc = L.space space1 empty empty

-- | Run the provided parser, then consume any trailing spaces.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Collect a contiguous list of non-space characters into a @Text@, then consume any trailing spaces.
-- Requires that a space trails the identifier.
ident :: Parser Text
ident = lexeme $ toText <$> takeWhile1P Nothing (not . isSpace)
