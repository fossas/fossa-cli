module App.Fossa.VSI.DynLinked.Internal (
  listLocalDependencies,
  lddParseLocalDependencies,
  lddParseDependency,
  LocalDependency (..),
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Monad (unless, void)
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text, isInfixOf)
import Data.Void (Void)
import Effect.Exec (Exec)
import Effect.Exec qualified as Exec
import Path (Abs, File, Path)
import Path qualified as P
import System.Info qualified as SysInfo
import Text.Megaparsec (Parsec, between, empty, eof, many, satisfy, try, (<|>))
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as L

-- | Report the dynamically linked dependencies of the given file.
-- This is currently a stub for non-linux targets: on these systems an empty set is reported.
listLocalDependencies :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs File -> m (Set (Path Abs File))
listLocalDependencies file | SysInfo.os == "linux" = do
  deps <- Exec.execParser lddParseLocalDependencies (P.parent file) $ Exec.Command "ldd" [toText $ P.toFilePath file] Exec.Never
  pure . Set.fromList $ map localDependencyPath deps
listLocalDependencies _ = pure Set.empty

type Parser = Parsec Void Text

data LocalDependency = LocalDependency
  { localDependencyName :: Text
  , localDependencyPath :: (Path Abs File)
  }
  deriving (Show, Eq, Ord)

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
lddParseLocalDependencies :: Parser [LocalDependency]
lddParseLocalDependencies =
  catMaybes
    <$> many
      ( try lddConsumeSyscallLib
          <|> try lddConsumeLinker
          <|> try lddParseDependency
      )
      <* eof

lddParseDependency :: Parser (Maybe LocalDependency)
lddParseDependency = Just <$> (LocalDependency <$> (linePrefix *> ident) <* symbol "=>" <*> path <* printedHex)

-- | The userspace library for system calls appears as the following in @ldd@ output:
--
-- > linux-vdso.so.1 (0x00007fff7e567000)
--
-- We want to ignore it, so just consume it:
-- this parser always returns @Nothing@ after having consumed the line.
lddConsumeSyscallLib :: Parser (Maybe LocalDependency)
lddConsumeSyscallLib = do
  _ <- linePrefix <* symbol "linux-vdso.so.1" <* symbol "=>" <* printedHex
  pure Nothing

-- | The linker appears as the following in @ldd@ output:
--
-- > /lib64/ld-linux-x86-64.so.2 (0x00007f4232cc1000)
--
-- We want to ignore it, so just consume it:
-- this parser always returns @Nothing@ after having consumed the line.
--
-- Where the linker is stored and its postfix vary by architecture.
-- For that reason, this function successfully parses (and thereby ignores)
-- any line that begins with an @ident@ containing @/ld-linux@.
lddConsumeLinker :: Parser (Maybe LocalDependency)
lddConsumeLinker = do
  name <- linePrefix *> ident
  unless ("/ld-linux" `isInfixOf` name) $ fail "try another parser"
  _ <- printedHex
  pure Nothing

-- | Lines may be prefixed by arbitrary spaces.
linePrefix :: Parser ()
linePrefix = sc

-- | We don't care about the memory address, so just consume it.
printedHex :: Parser ()
printedHex = void . lexeme . between (char '(') (char ')') $ many (satisfy (/= ')'))

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
  case P.parseAbsFile filepath of
    Left err -> fail (show err)
    Right a -> pure a
