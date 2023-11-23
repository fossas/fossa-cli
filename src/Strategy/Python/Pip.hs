{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Strategy.Python.Pip (Package (..), PackageMetadata (..), getPackages, pipShowParser) where

import Control.Effect.Diagnostics (Diagnostics, recover)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor (void)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  Has,
  execJson,
  execParser,
 )
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

data Package = Package
  { pkgName :: Text
  , pkgVersion :: Text
  , requires :: [Package]
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

pythonPip :: [Text] -> Command
pythonPip args =
  Command
    { cmdName = "python"
    , cmdArgs = ["-m", "pip", "--require-virtualenv"] <> args
    , cmdAllowErr = Never
    }

-- | Executes pip list.
execPipList :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m [InstalledPackage]
execPipList scanDir = do
  execJson scanDir $ pythonPip ["list", "--format=json"]

-- | Executes pip show.
execPipShow :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> [Text] -> m [PackageMetadata]
execPipShow scanDir packages = do
  -- pip show displays a RFC-compliant mail header format
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
      requires <- filter (/= "") <$> (Text.splitOn ", " <$> parseField "Requires")
      pure $ PackageMetadata name version requires

    parseField :: Text -> Parser Text
    parseField field = skipManyTill anySingle $ symbol field *> symbol ":" *> ident

    -- | Consume only spaces.
    sc :: Parser ()
    sc = L.space (void $ some (char ' ')) empty empty

    -- | Run the provided parser, then consume any trailing spaces.
    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme sc

    -- | Parse for the provided symbol, then consume any trailing spaces.
    symbol :: Text -> Parser Text
    symbol = L.symbol sc

    -- | Collect a contiguous list of characters into a @Text@, then consume any trailing spaces.
    -- Requires that a space trails the identifier.
    ident :: Parser Text
    ident = lexeme $ toText <$> takeWhileP Nothing (/= '\n')


-- | Maps package metadata into a package which contains the package's transitive dependencies.
toPackages :: [PackageMetadata] -> [Package]
toPackages packagesInfo = do
  map (toPackage [] packagesInfo) packagesInfo
 where
    toPackage seen metadata (PackageMetadata name version requires) =
      Package
        name
        version
        (mapMaybe (findPackageByName seen metadata) requires)

    findPackageByName seen metadata pkgName = do
      let pkg = find (\(PackageMetadata name _ _) -> name == pkgName) metadata
      case pkg of
        Just p -> do
          if p `notElem` seen
            then Just $ toPackage (seen ++ [p]) metadata p
            else Nothing
        Nothing -> Nothing

-- | Generates a list of packages and their transitive dependencies that are currently installed within the active environment.
getPackages :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Maybe [Package])
getPackages scanDir = do
  recover $ do
    installedPackages <- execPipList scanDir
    toPackages <$> execPipShow scanDir (map (\InstalledPackage{name} -> name) installedPackages)
