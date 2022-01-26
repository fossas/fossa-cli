module App.Fossa.VSI.DynLinked.Internal.Lookup.RPM (
  lookupDependencies,
  parseCommand,
  LinuxPackageSubset (..),
) where

import App.Fossa.VSI.DynLinked.Types (DynamicDependency (..), LinuxPackageManager (LinuxPackageManagerRPM), ResolvedLinuxPackage (..))
import App.Fossa.VSI.DynLinked.Util (runningLinux)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.String.Conversion (decodeUtf8, toText)
import Data.Text (Text, isInfixOf, strip)
import Data.Void (Void)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execParser, execThrow)
import Path (Abs, Dir, File, Path, toFilePath)
import Text.Megaparsec (Parsec, empty, many, option, satisfy, try, (<|>))
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

data LinuxPackageSubset = LinuxPackageSubset
  { linuxPackageID :: Text
  , linuxPackageRevision :: Text
  , linuxPackageArch :: Text
  , linuxPackageDistroEpoch :: Maybe Text
  }
  deriving (Show, Eq)

-- | The idea here is that we look up what paths we can with RPM and turn them into @DynamicDependency@.
-- We then hand back leftovers and lookup results for the next resolution function.
lookupDependencies :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> [Path Abs File] -> m ([Path Abs File], [DynamicDependency])
lookupDependencies _ files | not runningLinux = pure (files, [])
lookupDependencies root files = partitionEithers <$> traverse (tryLookup root) files

tryLookup :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> Path Abs File -> m (Either (Path Abs File) DynamicDependency)
tryLookup root file = fmap (maybeToRight file) . runMaybeT $ do
  pkgName <- MaybeT $ rpmqf root file
  meta <- MaybeT $ rpmqi root pkgName

  -- TODO: I need to refactor the types to remove distro name & version from packages.
  -- for now, leave this undefined until I've merged in flight PRs so I don't have to deal with conflicts.

  pure . DynamicDependency file . Just $ ResolvedLinuxPackage LinuxPackageManagerRPM undefined

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight df right = case right of
  Just a -> Right a
  Nothing -> Left df

rpmqf :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> Path Abs File -> m (Maybe Text)
rpmqf _ _ | not runningLinux = pure Nothing
rpmqf root file = do
  content <- decodeUtf8 . BL.toStrict <$> execThrow root (Command "rpm" ["-qf", toText $ toFilePath file] Never)
  if "not owned by any package" `isInfixOf` content
    then pure Nothing
    else pure . Just $ strip content

rpmqi :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> Text -> m (Maybe LinuxPackageSubset)
rpmqi _ _ | not runningLinux = pure Nothing
rpmqi root name = execParser parseCommand root $ Command "rpm" ["-qi", name] Never

parseCommand :: Parser (Maybe LinuxPackageSubset)
parseCommand = try consumePackageNotFound <|> fmap Just parseMeta

consumePackageNotFound :: Parser (Maybe LinuxPackageSubset)
consumePackageNotFound = do
  _ <- symbol "package" <* ident <* symbol "is" <* symbol "not" <* symbol "installed"
  pure Nothing

-- | To keep things simple for now, assume fields always appear in predictable order.
-- if this turns out to be incorrect, we should parse the rpm db directly, like syft does.
parseMeta :: Parser LinuxPackageSubset
parseMeta = do
  name <- parseField "Name"
  epoch <- try . option Nothing $ Just <$> parseField "Epoch"
  version <- parseField "Version"
  release <- parseField "Release"
  arch <- parseField "Architecture"
  pure $ LinuxPackageSubset name (version <> "-" <> release) arch epoch

parseField :: Text -> Parser Text
parseField field = symbol field *> symbol ":" *> ident

-- | Consume spaces.
sc :: Parser ()
sc = L.space space1 empty empty

-- | Run the provided parser, then consume any trailing spaces.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse for the provided symbol, then consume any trailing spaces.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Collect a contiguous list of non-space characters into a @Text@, then consume any trailing spaces.
-- Requires that a space trails the identifier.
ident :: Parser Text
ident = lexeme $ toText <$> many (satisfy $ not . isSpace)
