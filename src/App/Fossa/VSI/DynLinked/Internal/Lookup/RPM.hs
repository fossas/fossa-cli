module App.Fossa.VSI.DynLinked.Internal.Lookup.RPM (
  rpmTactic,
  rpmParseQueryPackageInfo,
) where

import App.Fossa.VSI.DynLinked.Types (DynamicDependency (..), LinuxPackageManager (..), LinuxPackageMetadata (..), ResolvedLinuxPackage (..))
import App.Fossa.VSI.DynLinked.Util (runningLinux)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Char (isSpace)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execParser)
import Path (Abs, Dir, File, Path)
import Text.Megaparsec (MonadParsec (eof), Parsec, empty, option, takeWhile1P, try)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

rpmTactic ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  m (Maybe DynamicDependency)
rpmTactic root file | runningLinux = do
  packageForFile root file >>= \case
    Nothing -> pure Nothing
    Just name -> do
      meta <- context ("Parse metadata for package " <> toText (show name) <> ", which owns file " <> toText (show file)) $ packageMeta root name
      pure (DynamicDependency file . Just . ResolvedLinuxPackage LinuxPackageManagerRPM <$> meta)
rpmTactic _ _ = pure Nothing

packageForFile :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> Path Abs File -> m (Maybe Text)
packageForFile _ _ | not runningLinux = pure Nothing
packageForFile root file = Just <$> execParser rpmParseQueryFile root (rpmQueryFileCommand file)

rpmQueryFileCommand :: Path Abs File -> Command
rpmQueryFileCommand file =
  Command
    { cmdName = "rpm"
    , cmdArgs = ["-qf", toText file]
    , cmdAllowErr = Never
    }

-- | Parse @rpm -qf@ output.
-- Example:
--
-- > rpm -qf /lib64/libc.so.6
-- > glibc-2.28-151.el8.x86_64\n
-- > ^^^^^^^^^^^^^^^^^^^^^^^^^ we want this whole output (but we don't want the trailing newline)
rpmParseQueryFile :: Parser Text
rpmParseQueryFile = do
  pkg <- takeWhile1P Nothing (const True) <* eof
  pure $ Text.strip pkg

packageMeta :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> Text -> m (Maybe LinuxPackageMetadata)
packageMeta _ _ | not runningLinux = pure Nothing
packageMeta root name = Just <$> execParser rpmParseQueryPackageInfo root (rpmQueryPackageInfoCommand name)

rpmQueryPackageInfoCommand :: Text -> Command
rpmQueryPackageInfoCommand packageName =
  Command
    { cmdName = "rpm"
    , cmdArgs = ["-qi", packageName]
    , cmdAllowErr = Never
    }

-- | Parse @rpm -qi@ output.
--
-- To keep things simple for now, assume fields always appear in predictable order.
-- if this turns out to be incorrect, we should parse the rpm db directly, like syft does.
--
-- Example output:
-- > rpm -qi glibc-2.28-151.el8.x86_64
-- > Name        : glibc
-- > Version     : 2.28
-- > Release     : 151.el8
-- > Architecture: x86_64
-- > Install Date: Wed Sep 15 14:17:28 2021
-- > Group       : Unspecified
-- > Size        : 15646740
-- > License     : LGPLv2+ and LGPLv2+ with exceptions and GPLv2+ and GPLv2+ with exceptions and BSD and Inner-Net and ISC and Public Domain and GFDL
-- > Signature   : RSA/SHA256, Thu Mar 11 21:46:42 2021, Key ID 05b555b38483c65d
-- > Source RPM  : glibc-2.28-151.el8.src.rpm
-- > Build Date  : Thu Mar 11 20:16:40 2021
-- > Build Host  : x86-01.mbox.centos.org
-- > Relocations : (not relocatable)
-- > Packager    : CentOS Buildsys <bugs@centos.org>
-- > Vendor      : CentOS
-- > URL         : http://www.gnu.org/software/glibc/
-- > Summary     : The GNU libc libraries
-- > Description :
-- > The glibc package contains standard libraries which are used by
-- > multiple programs on the system. In order to save disk space and
-- > memory, as well as to make upgrading easier, common system code is
-- > kept in one place and shared between programs. This particular package
-- > contains the most important sets of shared libraries: the standard C
-- > library and the standard math library. Without these two libraries, a
-- > Linux system will not function.
rpmParseQueryPackageInfo :: Parser LinuxPackageMetadata
rpmParseQueryPackageInfo = do
  name <- parseField "Name"
  epoch <- try . option Nothing $ Just <$> parseField "Epoch"
  version <- parseField "Version"
  release <- parseField "Release"
  arch <- parseField "Architecture"
  pure $ LinuxPackageMetadata name (version <> "-" <> release) arch epoch

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
ident = lexeme $ toText <$> takeWhile1P Nothing (not . isSpace)
