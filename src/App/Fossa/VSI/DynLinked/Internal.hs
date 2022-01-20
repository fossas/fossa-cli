module App.Fossa.VSI.DynLinked.Internal (
  listLocalDependencies,
  parseLocalDependencies,
  parseLine,
  LocalDependency (..),
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Data.Char (isSpace)
import Data.Set (Set)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Effect.Exec (Exec)
import Path (Abs, File, Path, parseAbsFile)
import Text.Megaparsec (Parsec, between, empty, eof, many, satisfy)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as L

listLocalDependencies :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs File -> m (Set (Path Abs File))
listLocalDependencies file = undefined

type Parser = Parsec Void Text

data LocalDependency = LocalDependency Text (Path Abs File) deriving (Show, Eq, Ord)

-- | Parse output from 'ldd' into a list of dependencies on disk.
--
-- Output from 'ldd' looks like this:
--
-- >  linux-vdso.so.1 =>  (0x00007ffc28d59000)
-- >  libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)
-- >  /lib64/ld-linux-x86-64.so.2 (0x00007fbea9e52000)
--
-- Each line is in the form @{spaces}{library name}{spaces}{literal =>}{spaces}{library path on disk}{spaces}{memory address}@.
--   * The memory address is ignored.
--   * We consider each line to form a tuple of @({Name}, {Path})@.
--     * Any line that doesn't form a complete tuple is considered a parsing error (with the below exceptions).
--
-- Exceptions:
--   * Lines with the library name @linux-vdso.so.1@ is ignored.
--   * Lines with the literal @/ld-linux@ in them are ignored.
--
-- The above rules mean that the output above would evaluate to the following result:
--
-- > Set.fromList [$(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")]
parseLocalDependencies :: Parser [LocalDependency]
parseLocalDependencies = many parseLine <* eof

parseLine :: Parser LocalDependency
parseLine = LocalDependency <$> (prefix *> name) <* symbol "=>" <*> path <* addr
  where
    prefix = sc -- lines may be prefixed by arbitrary spaces
    name = ident -- the name is a plain identifier
    addr = lexeme . between (char '(') (char ')') $ many (satisfy (/= ')'))

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

-- | Parse a @Path Abs File@, then consume any trailing spaces.
path :: Parser (Path Abs File)
path = lexeme $ do
  filepath <- many (satisfy $ not . isSpace)
  case parseAbsFile filepath of
    Left err -> fail (show err)
    Right a -> pure a
