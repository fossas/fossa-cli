module App.Fossa.VSI.DynLinked.Internal.Binary (
  dynamicLinkedDependencies,
  lddParseLocalDependencies,
  lddParseDependency,
  LocalDependency (..),
) where

import App.Fossa.VSI.DynLinked.Util (runningLinux)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, context, recover, renderDiagnostic, warnOnErr)
import Control.Effect.Reader (Reader)
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (ToString (toString), toText)
import Data.Text (Text)
import Data.Void (Void)
import Discovery.Filters (AllFilters)
import Discovery.Walk (WalkStep (WalkContinue), walkWithFilters')
import Effect.Exec (AllowErr (Never), Command (..), Exec, execParser)
import Effect.Logger (Logger, logDebug, pretty)
import Effect.ReadFS (ReadFS)
import Errata (Errata (..))
import Path (Abs, Dir, File, Path, parent, parseAbsFile)
import Path.Extra (SomeResolvedPath (..))
import Text.Megaparsec (Parsec, between, empty, eof, many, optional, takeWhile1P, try, (<|>))
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as L

-- | Report the list of dynamically linked dependencies of the target executable as a set of file paths.
-- This is currently a stub for non-linux targets: on these systems an empty set is reported.
dynamicLinkedDependencies ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has (Reader AllFilters) sig m
  ) =>
  SomeResolvedPath ->
  m (Set (Path Abs File))
dynamicLinkedDependencies target | runningLinux = case target of
  ResolvedDir dir -> dynamicLinkedDependenciesRecursive dir
  ResolvedFile file -> dynamicLinkedDependenciesSingle file
dynamicLinkedDependencies _ = pure Set.empty

dynamicLinkedDependenciesRecursive ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m (Set (Path Abs File))
dynamicLinkedDependenciesRecursive root = context "Recursively discover dynamic binaries" . flip walkWithFilters' root $ \dir _ files -> do
  -- Log this so that users running in debug mode see the CLI working during long-running recursive traversals.
  logDebug . pretty $ "[DETECT-DYNAMIC] Inspecting binaries in directory: " <> toText (show dir)
  deps <- traverse resolveSingleWarnOnErr files
  pure (Set.unions $ catMaybes deps, WalkContinue)
  where
    resolveSingleWarnOnErr :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs File -> m (Maybe (Set (Path Abs File)))
    resolveSingleWarnOnErr target = recover . warnOnErr (SkippingDynamicDep target) $ dynamicLinkedDependenciesSingle target

dynamicLinkedDependenciesSingle ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  ) =>
  Path Abs File ->
  m (Set (Path Abs File))
dynamicLinkedDependenciesSingle file = context ("Inspect " <> toText (show file) <> " for dynamic dependencies") $ do
  deps <- execParser lddParseLocalDependencies (parent file) $ lddCommand file
  pure . Set.fromList $ map localDependencyPath deps

newtype SkippingDynamicDep = SkippingDynamicDep (Path Abs File)
instance ToDiagnostic SkippingDynamicDep where
  renderDiagnostic (SkippingDynamicDep target) = do
    let header = "Skipping dynamic analysis for target: " <> toText (show target)
    Errata (Just header) [] Nothing

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
-- Sometimes, the => is optional. Why this happens isn't well documented and I haven't found anything different
-- between the cases where => is included and when it's excluded.
--
-- We want to ignore these, so just consume them:
-- this parser always returns @Nothing@ after having consumed the line.
lddConsumeSyscallLib :: Parser (Maybe LocalDependency)
lddConsumeSyscallLib = do
  _ <- linePrefix <* ident <* optional (symbol "=>") <* printedHex
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
printedHex = void . lexeme . between (char '(') (char ')') $ takeWhile1P Nothing (/= ')')

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
  filepath <- toString <$> takeWhile1P Nothing (not . isSpace)
  case parseAbsFile filepath of
    Left err -> fail (show err)
    Right a -> pure a
