module App.Fossa.VSI.DynLinked.Internal.Binary (
  dynamicLinkedDependencies,
  lddParseLocalDependencies,
  lddParseDependency,
  LocalDependency (..),
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, recover)
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execParser)
import Path (Abs, File, Path, parent, parseAbsFile)
import System.Info qualified as SysInfo
import Text.Megaparsec (Parsec, between, empty, eof, many, satisfy, try, (<|>))
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as L

-- | Report the list of dynamically linked dependencies of the target executable as a set of file paths.
-- This is currently a stub for non-linux targets: on these systems an empty set is reported.
dynamicLinkedDependencies :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs File -> m (Set (Path Abs File))
dynamicLinkedDependencies file | SysInfo.os == "linux" = do
  -- If the target isn't a dynamically linked dependency, @ldd@ exits with code 1.
  -- Handle this and any other issues by stashing them in @Diagnostics@ so we can just warn the user about it at the end.
  deps <- recover . execParser lddParseLocalDependencies (parent file) $ lddCommand file
  case deps of
    Nothing -> pure Set.empty
    Just paths -> pure . Set.fromList $ map localDependencyPath paths
dynamicLinkedDependencies _ = pure Set.empty

lddCommand :: Path Abs File -> Command
lddCommand binaryPath =
  Command
    { cmdName = "ldd"
    , cmdArgs = [toText binaryPath]
    , cmdAllowErr = Never
    }

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
-- Or this:
--
-- >  /lib/ld-musl-x86_64.so.1 (0x7f2ca52da000)
-- >  libc.musl-x86_64.so.1 => /lib/ld-musl-x86_64.so.1 (0x7f2ca52da000)
--
-- Each line is in the form @{name}{literal =>}{path}{memory address}@.
--   * The memory address is ignored.
--   * We consider each line to form a tuple of @({name}, {path})@.
--     * Any line that doesn't form a complete tuple is ignored.
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
-- > linux-vdso.so.1 => (0x00007ffc28d59000)
--
-- In other words, such libraries are in the format @{name}{literal =>}{memory address}@.
--
-- We want to ignore these, so just consume them:
-- this parser always returns @Nothing@ after having consumed the line.
lddConsumeSyscallLib :: Parser (Maybe LocalDependency)
lddConsumeSyscallLib = do
  _ <- linePrefix <* ident <* symbol "=>" <* printedHex
  pure Nothing

-- | The linker appears as the following in @ldd@ output:
--
-- > /lib64/ld-linux-x86-64.so.2 (0x00007f4232cc1000)
--
-- In other words, such libraries are in the format @{path}{memory address}@.
--
-- We want to ignore these, so just consume them:
-- this parser always returns @Nothing@ after having consumed the line.
lddConsumeLinker :: Parser (Maybe LocalDependency)
lddConsumeLinker = do
  _ <- linePrefix <* path <* printedHex
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
-- We don't support spaces in the file path due to C library naming conventions.
-- From `https://amir.rachum.com/blog/2016/09/17/shared-libraries`:
--
-- >  Notice that we called the shared library librandom.so.
-- >  This is not arbitrary - shared libraries should be called lib<name>.so for them to link properly later on.
--
-- Working around this for specifically @ldd@ parsing is difficult as well considering @ldd@ uses spaces to separate
-- fields in its output- if this becomes a problem we'll most likely need to reach for reading the ELF sections ourselves.
path :: Parser (Path Abs File)
path = lexeme $ do
  filepath <- many (satisfy $ not . isSpace)
  case parseAbsFile filepath of
    Left err -> fail (show err)
    Right a -> pure a
