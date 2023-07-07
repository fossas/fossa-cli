module Strategy.Conan.Version (
  guardConanVersion2Gt,
  conanVersion,
) where

import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Monad (void)
import Data.SemVer (Version, fromText, toText, version)
import Data.SemVer.Constraint (Constraint (..), satisfies)
import Data.Text (Text)
import Data.Void (Void)
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  Has,
  execParser,
 )
import Path (Abs, Dir, Path)
import Text.Megaparsec (
  MonadParsec (takeWhile1P),
  Parsec,
  empty,
  some,
  (<|>),
 )
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

sc :: Parser ()
sc = Lexer.space (void $ some $ char ' ' <|> char '\t') empty empty

symbol :: Text -> Parser Text
symbol = Lexer.symbol sc

-- | Parses conan version from output of @conanVersionCmd@
--
-- >> parseTest majorConanVersion "conan version v1.2.4"
-- > Version {...}
conanVersion :: Parser Version
conanVersion = do
  void $ symbol "conan" <* symbol "version"
  version' <- fromText <$> (takeWhile1P (Just "conan version") (/= ' '))
  case version' of
    Left err -> fail err
    Right parsedVersion -> pure parsedVersion

-- | Represents `conan --version`.
-- Retrieve conan version. This commend is not documented,
-- on docs or on conan --help, but exists for Conan v1, and v2.
--
-- >> conan --version
-- > conan version 2.0.5
conanVersionCmd :: Command
conanVersionCmd =
  Command
    { cmdName = "conan"
    , cmdArgs = ["--version"]
    , cmdAllowErr = Never
    }

guardConanVersion2Gt :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m ()
guardConanVersion2Gt dir = do
  conanVer <- execParser conanVersion dir conanVersionCmd
  if satisfies conanVer (CGt version_2_0_0)
    then pure ()
    else fatalText $ "Expected conan version greater than 2.0.0, but recieved: " <> toText conanVer
  where
    version_2_0_0 :: Version
    version_2_0_0 = version 2 0 0 [] []
