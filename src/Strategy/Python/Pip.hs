{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Strategy.Python.Pip (PythonPackage (..), PackageMetadata (..), getPackages, pipShowParser) where

import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, recover, warnOnErr)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor (void)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Diag.Diagnostic (ToDiagnostic (..))
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  Has,
  execJson,
  execParser,
 )
import Effect.Logger (vsep)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text
data InstalledPackage = InstalledPackage
  { name :: Text
  , version :: Text
  }
  deriving (Show, Eq, Ord, Generic, FromJSON)

data PackageMetadata = PackageMetadata Text Text [Text]
  deriving (Show, Eq, Ord)

data PythonPackage = PythonPackage
  { pkgName :: Text
  , pkgVersion :: Text
  , requires :: [PythonPackage]
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data PipListCommandFailed = PipListCommandFailed

instance ToDiagnostic PipListCommandFailed where
  renderDiagnostic PipListCommandFailed =
    vsep
      [ "Failed to run pip command"
      ]

pythonPip :: [Text] -> Command
pythonPip args =
  Command
    { cmdName = "python"
    , cmdArgs =
        [ "-m"
        , "pip"
        , "--disable-pip-version-check" -- suppresses pip version check warning
        ]
          <> args
    , cmdAllowErr = Never
    }

-- | Executes pip list which will return a json data with name and version.
-- See https://pip.pypa.io/en/stable/cli/pip_list/ for options / output.
execPipList :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m [InstalledPackage]
execPipList scanDir =
  execJson scanDir $ pythonPip ["list", "--format=json"]

-- | Executes pip show, we expect stdout to be a RFC-compliant mail header format.
-- See https://pip.pypa.io/en/stable/cli/pip_show/ for options / output.
execPipShow :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> [Text] -> m [PackageMetadata]
execPipShow scanDir packages =
  execParser pipShowParser scanDir (pythonPip $ ["show"] <> packages)

-- | Parses package name, version and requires from the output from pip show.
pipShowParser :: Parser [PackageMetadata]
pipShowParser = many (try parseMetadata <|> try parseSection)
  where
    parseSection :: Parser PackageMetadata
    parseSection = do
      _ <- L.nonIndented sc $ symbol "---"
      try parseMetadata

    parseMetadata :: Parser PackageMetadata
    parseMetadata = do
      name <- parseField "Name"
      version <- parseField "Version"
      requires <- filter (/= "") . Text.splitOn ", " <$> (parseField "Requires")
      pure $ PackageMetadata name version requires

    parseField :: Text -> Parser Text
    parseField field = skipManyTill anySingle $ symbol field *> symbol ":" *> ident

    -- \| Consume only spaces.
    sc :: Parser ()
    sc = L.space (void $ some (char ' ')) empty empty

    -- \| Run the provided parser, then consume any trailing spaces.
    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme sc

    -- \| Parse for the provided symbol, then consume any trailing spaces.
    symbol :: Text -> Parser Text
    symbol = L.symbol sc

    -- \| Collect a contiguous list of characters into a @Text@, then consume any trailing spaces.
    -- Requires that a space trails the identifier.
    ident :: Parser Text
    ident = lexeme $ toText <$> takeWhileP Nothing (/= '\n')

-- | Maps package metadata into a package which contains the package's transitive dependencies.
toPackages :: [PackageMetadata] -> [PythonPackage]
toPackages packagesInfo = do
  map (toPackage [] packagesInfo) packagesInfo
  where
    toPackage :: [PackageMetadata] -> [PackageMetadata] -> PackageMetadata -> PythonPackage
    toPackage seen metadata (PackageMetadata name version requires) =
      PythonPackage
        name
        version
        (mapMaybe (findPackageByName seen metadata) requires)

    findPackageByName :: [PackageMetadata] -> [PackageMetadata] -> Text -> Maybe PythonPackage
    findPackageByName seen metadata pkgName =
      case find (\(PackageMetadata name _ _) -> name == pkgName) metadata of
        Just p -> do
          if p `notElem` seen
            then Just $ toPackage (seen ++ [p]) metadata p
            else Nothing
        Nothing -> Nothing

-- | Generates a list of packages and their transitive dependencies that are currently installed within the active environment.
getPackages :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Maybe [PythonPackage])
getPackages scanDir =
  recover . warnOnErr PipListCommandFailed $ do
    installedPackages <- execPipList scanDir
    toPackages <$> execPipShow scanDir (map (\InstalledPackage{name} -> name) installedPackages)
