-- | A 'Result' type, suitable for recording errors and warnings that occurred
-- during a computation. It's like @Either@, but much more useful.
--
-- To avoid binding pollution, operations on the base 'Result' type are omitted
-- from this module. For example, while @warn@ is an important primitive when
-- building up 'Result' values, that name would also conflict with the @warn@
-- from our @Diagnostics@ effect. This module exclusively exports the essential
-- result-related datatypes and destructors on those datatypes.
--
-- Instead, those essential operations are defined for the @ResultT@ monad
-- transformer, which lives in the 'Diag.Monad' module. The operations available
-- in that module provide color/semantics to the datatypes defined in this
-- module. Make sure to read the documentation in 'Diag.Monad' as well.
module Diag.Result (
  -- * Result type
  Result (..),
  EmittedWarn (..),
  ErrGroup (..),
  ErrWithStack (..),
  Stack (..),

  -- * Diagnostics
  SomeErr (..),
  SomeWarn (..),
  ErrCtx (..),

  -- * Helpers
  resultToMaybe,

  -- * Rendering
  renderFailure,
  renderSuccess,
  renderFailureWithoutWarnings,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
import GHC.Show (showLitString)
import Prettyprinter
import Prettyprinter.Render.Terminal

-- | The result of some computation producing either:
--
-- - A Failure, containing the group of errors that directly led to the failure ('ErrGroup').
--
-- - A Success, containing the result value @a@.
--
-- Both data constructors also contain the list of warning emitted during
-- the computation (@['EmittedWarn']@). More-recent warnings appear first
-- in the list.
--
-- The Applicative and Monad instances on Result carry different semantics, and
-- aren't legal according to the haskell police (please don't tell them).
--
-- When building up a Result in the Applicative style with '<*>', we accumulate
-- Failures encountered into a single Failure.
--
-- When building up a result in the Monadic style with '>>=', we short-circuit
-- on the first Failure.
data Result a = Failure [EmittedWarn] ErrGroup | Success [EmittedWarn] a
  deriving (Show)

-- | A warning emitted during a computation
--
-- It has three constructors:
--
-- - 'StandaloneWarn', which contains only a warning
--
-- - 'WarnOnErrGroup', emitted when recovering from a Failure whose ErrGroup
--   contains at least one warning
--
-- - 'IgnoredErrGroup', emitted when recovering from a Failure whose ErrGroup
--   contains no warnings
data EmittedWarn
  = StandaloneWarn SomeWarn
  | WarnOnErrGroup (NonEmpty SomeWarn) [ErrCtx] (NonEmpty ErrWithStack)
  | IgnoredErrGroup [ErrCtx] (NonEmpty ErrWithStack)
  deriving (Show)

-- | An error, or group of errors, that occurred during a computation that led
-- to a Failure. An ErrGroup can have warnings and error context attached.
data ErrGroup = ErrGroup [SomeWarn] [ErrCtx] (NonEmpty ErrWithStack)
  deriving (Show)

instance Semigroup ErrGroup where
  (<>) :: ErrGroup -> ErrGroup -> ErrGroup
  ErrGroup sws ectx nee <> ErrGroup sws' ectx' nee' = ErrGroup (sws <> sws') (ectx <> ectx') (nee <> nee')

-- | An error with an associated stacktrace
data ErrWithStack = ErrWithStack Stack SomeErr
  deriving (Show)

-- | A stacktrace
newtype Stack = Stack [Text]
  deriving (Show)

-- | Dictates whether or not we want to show traceback
data TracebackStyle = None | Default
  deriving (Show)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap _ (Failure ws eg) = Failure ws eg
  fmap f (Success ws a) = Success ws (f a)

instance Applicative Result where
  (<*>) :: Result (a -> b) -> Result a -> Result b
  Success ws f <*> Success ws' a = Success (ws' <> ws) (f a)
  Success ws _ <*> Failure ws' eg' = Failure (ws' <> ws) eg'
  Failure ws eg <*> Success ws' _ = Failure (ws' <> ws) eg
  Failure ws eg <*> Failure ws' eg' = Failure (ws' <> ws) (eg' <> eg)

  pure :: a -> Result a
  pure = Success []

instance Monad Result where
  (>>=) :: Result a -> (a -> Result b) -> Result b
  Failure ws eg >>= _ = Failure ws eg
  Success ws a >>= k =
    case k a of
      Failure ws' eg' -> Failure (ws' <> ws) eg'
      Success ws' b' -> Success (ws' <> ws) b'

---------- Diagnostics

-- | Some warning type. Right now, this just requires a ToDiagnostic instance for the type.
data SomeWarn where
  SomeWarn :: ToDiagnostic diag => diag -> SomeWarn

instance Show SomeWarn where
  showsPrec p (SomeWarn w) = showParen (p > 10) $ showString "SomeWarn " . diagToShowS w

-- | Some error type. Right now, this just requires a ToDiagnostic instance for the type.
data SomeErr where
  SomeErr :: ToDiagnostic diag => diag -> SomeErr

instance Show SomeErr where
  showsPrec p (SomeErr e) = showParen (p > 10) $ showString "SomeErr " . diagToShowS e

-- | Some error context type. Right now, this just requires a ToDiagnostics instance for the type.
data ErrCtx where
  ErrCtx :: ToDiagnostic diag => diag -> ErrCtx

instance Show ErrCtx where
  showsPrec p (ErrCtx c) = showParen (p > 10) $ showString "ErrCtx " . diagToShowS c

diagToShowS :: ToDiagnostic diag => diag -> ShowS
diagToShowS = showWrapped '"' . showLitString . show . renderDiagnostic
  where
    showWrapped :: Char -> ShowS -> ShowS
    showWrapped c str = showChar c . str . showChar c

---------- Helpers

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Success _ a) = Just a
resultToMaybe (Failure _ _) = Nothing

---------- Rendering Failure and Success

-- | renderFailure turns a list of accumulated warnings and the fatal ErrGroup
-- from a Failure into a message suitable for logging
--
-- renderFailure displays all types of emitted warnings.
renderFailure :: [EmittedWarn] -> ErrGroup -> Doc AnsiStyle -> Doc AnsiStyle
renderFailure ws (ErrGroup _ ectx es) headerDoc = header headerDoc <> renderErrCtxs ectx <> renderedErrsWithAnnotation <> renderedPossibleErrs
  where
    renderedErrsWithAnnotation :: Doc AnsiStyle
    renderedErrsWithAnnotation = section "Relevant errors" $ subsection "Error" $ renderErrs es Default

    renderedPossibleErrs :: Doc AnsiStyle
    renderedPossibleErrs =
      case ws of
        [] -> emptyDoc
        _ ->
          section "Possibly-related warnings" $
            unannotatedSubsection (map (renderEmittedWarn Default) ws)

renderFailureWithoutWarnings :: ErrGroup -> Doc AnsiStyle -> Doc AnsiStyle
renderFailureWithoutWarnings (ErrGroup _ ectx es) headerDoc = header headerDoc <> renderErrCtxs ectx <> section "Relevant errors" (subsection "Error" $ renderErrs es None)

-- | renderSuccess turns a list of warnings from a Success into a message
-- suitable for logging
--
-- renderSuccess only displays warnings that aren't 'IgnoredErrGroup'.
-- IgnoredErrGroup warnings are noisy: they are the errors we recovered from
-- without attaching an explicit warning. When the action succeeds, those errors
-- don't matter.
--
-- when all errors in the list are IgnoredErrGroup, or the provided list is
-- empty, this returns Nothing
renderSuccess :: [EmittedWarn] -> Doc AnsiStyle -> Maybe (Doc AnsiStyle)
renderSuccess ws headerDoc =
  case notIgnoredErrs of
    [] -> Nothing
    ws' -> Just $ header headerDoc <> unannotatedSubsection (map (renderEmittedWarn Default) ws')
  where
    notIgnoredErrs :: [EmittedWarn]
    notIgnoredErrs = filter (not . isIgnoredErrGroup) ws

    isIgnoredErrGroup :: EmittedWarn -> Bool
    isIgnoredErrGroup IgnoredErrGroup{} = True
    isIgnoredErrGroup _ = False

---------- Rendering a collection of Result components: [ErrCtx], NonEmpty ErrWithStack

renderErrCtxs :: [ErrCtx] -> Doc AnsiStyle
renderErrCtxs errCtxs = case errCtxs of
  [] -> emptyDoc
  _ -> section "Details" (vsep (map (\ctx -> renderErrCtx ctx <> line) errCtxs))

renderErrs :: NonEmpty ErrWithStack -> TracebackStyle -> [Doc AnsiStyle]
renderErrs es tracebackStyle = map (`renderErrWithStack` tracebackStyle) (NE.toList es)

---------- Rendering individual Result components: ErrCtx, EmittedWarn, SomeWarn, ErrWithStack

renderErrCtx :: ErrCtx -> Doc AnsiStyle
renderErrCtx (ErrCtx ctx) = renderDiagnostic ctx

renderErrWithStack :: ErrWithStack -> TracebackStyle -> Doc AnsiStyle
renderErrWithStack (ErrWithStack (Stack stack) (SomeErr err)) tracebackStyle = do
  let traceback = case tracebackStyle of
        Default ->
          line
            <> line
            <> annotate (color Cyan) "Traceback:"
            <> line
            <> case stack of
              [] -> indent 2 "(none)"
              _ -> indent 2 (vsep (map (pretty . ("- " <>)) stack))
        None -> ""

  renderDiagnostic err <> traceback

renderEmittedWarn :: TracebackStyle -> EmittedWarn -> Doc AnsiStyle
renderEmittedWarn tracebackStyle (IgnoredErrGroup ectx es) = renderedCtx <> renderedErrors
  where
    renderedCtx =
      case ectx of
        [] -> emptyDoc
        _ ->
          (vsep (map (\ctx -> renderErrCtx ctx <> line) ectx))

    renderedErrors = subsection "Warning" (map (`renderErrWithStack` tracebackStyle) (NE.toList es))
renderEmittedWarn _ (StandaloneWarn (SomeWarn warn)) = renderDiagnostic warn
renderEmittedWarn tracebackStyle (WarnOnErrGroup ws ectx es) = renderedWarnings <> renderedCtx <> renderedErrors
  where
    renderedWarnings = vsep (map (\w -> renderSomeWarn w <> line) (NE.toList ws)) <> line

    renderedCtx =
      case ectx of
        [] -> emptyDoc
        _ ->
          section
            "Details"
            (vsep (map (\ctx -> renderErrCtx ctx <> line) ectx))

    renderedErrors = subsection "Warning" (map (`renderErrWithStack` tracebackStyle) (NE.toList es))

renderSomeWarn :: SomeWarn -> Doc AnsiStyle
renderSomeWarn (SomeWarn w) = renderDiagnostic w

---------- Rendering helpers

header :: Doc AnsiStyle -> Doc AnsiStyle
header name =
  annotate (color Yellow) "----------"
    <> line
    <> annotate (color Yellow) name
    <> line
    <> line

section :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
section name content =
  annotate (color Yellow) (">>> " <> name)
    <> line
    <> line
    <> indent 2 content
    <> line

subsection :: Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
subsection name = vsep . map (\single -> annotate (color Yellow) name <> line <> line <> indent 2 single <> line)

unannotatedSubsection :: [Doc AnsiStyle] -> Doc AnsiStyle
unannotatedSubsection = vsep . map (\single -> indent 2 single <> line)
