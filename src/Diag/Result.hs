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
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
import Prettyprinter
import Prettyprinter.Render.Terminal

-- FIXME: doc about more-recent errors and warnings appearing first
data Result a = Failure [EmittedWarn] ErrGroup | Success [EmittedWarn] a
  deriving (Show)

-- TODO: add UncaughtErrGroup constructor?
data EmittedWarn
  = StandaloneWarn SomeWarn
  | WarnOnErrGroup (NonEmpty SomeWarn) [ErrCtx] (NonEmpty ErrWithStack)
  | IgnoredErrGroup [ErrCtx] (NonEmpty ErrWithStack)
  deriving (Show)

data ErrGroup = ErrGroup [SomeWarn] [ErrCtx] (NonEmpty ErrWithStack)
  deriving (Show)

instance Semigroup ErrGroup where
  ErrGroup sws ectx nee <> ErrGroup sws' ectx' nee' = ErrGroup (sws <> sws') (ectx <> ectx') (nee <> nee')

data ErrWithStack = ErrWithStack Stack SomeErr
  deriving (Show)

newtype Stack = Stack [Text]
  deriving (Show)

instance Functor Result where
  fmap _ (Failure ws eg) = Failure ws eg
  fmap f (Success ws a) = Success ws (f a)

instance Applicative Result where
  Success ws f <*> Success ws' a = Success (ws' <> ws) (f a)
  Success ws _ <*> Failure ws' eg' = Failure (ws' <> ws) eg'
  Failure ws eg <*> Success ws' _ = Failure (ws' <> ws) eg
  Failure ws eg <*> Failure ws' eg' = Failure (ws' <> ws) (eg' <> eg)

  pure = Success []

instance Monad Result where
  Failure ws eg >>= _ = Failure ws eg
  Success ws a >>= k =
    case k a of
      Failure ws' eg' -> Failure (ws' <> ws) eg'
      Success ws' b' -> Success (ws' <> ws) b'

---------- Diagnostics

-- FIXME: kill Show instances?

data SomeWarn where
  SomeWarn :: ToDiagnostic diag => diag -> SomeWarn

instance Show SomeWarn where
  show (SomeWarn w) = "\"" <> show (renderDiagnostic w) <> "\""

data SomeErr where
  SomeErr :: ToDiagnostic diag => diag -> SomeErr

instance Show SomeErr where
  show (SomeErr e) = "\"" <> show (renderDiagnostic e) <> "\""

data ErrCtx where
  ErrCtx :: ToDiagnostic diag => diag -> ErrCtx

instance Show ErrCtx where
  show (ErrCtx c) = "\"" <> show (renderDiagnostic c) <> "\""

---------- Helpers

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Success _ a) = Just a
resultToMaybe (Failure _ _) = Nothing

---------- Rendering

-- FIXME: standalone warnings are rendered poorly
renderFailure :: [EmittedWarn] -> ErrGroup -> Doc AnsiStyle
renderFailure ws (ErrGroup _ ectx es) = header "An issue occurred" <> renderedCtx <> renderedErrs <> renderedPossibleErrs
  where
    renderedCtx :: Doc AnsiStyle
    renderedCtx =
      case ectx of
        [] -> emptyDoc
        _ -> section "Description" (vsep (map (\ctx -> renderErrCtx ctx <> line) ectx))

    renderedErrs :: Doc AnsiStyle
    renderedErrs =
      section "Relevant errors" $
        numbered "Error" (map renderErrWithStack (NE.toList es))

    renderedPossibleErrs :: Doc AnsiStyle
    renderedPossibleErrs =
      case ws of
        [] -> emptyDoc
        _ ->
          section "Possibly-related warnings" $
            numbered "Warning" (map renderEmittedWarn ws)

renderErrCtx :: ErrCtx -> Doc AnsiStyle
renderErrCtx (ErrCtx ctx) = renderDiagnostic ctx

renderErrWithStack :: ErrWithStack -> Doc AnsiStyle
renderErrWithStack (ErrWithStack (Stack stack) (SomeErr err)) =
  renderDiagnostic err
    <> line
    <> line
    <> annotate (color Cyan) "Traceback:"
    <> line
    <> case stack of
      [] -> indent 2 "(none)"
      _ -> indent 2 (vsep (map (pretty . ("- " <>)) (reverse stack)))

renderSuccess :: [EmittedWarn] -> Maybe (Doc AnsiStyle)
renderSuccess ws =
  case notIgnoredErrs of
    [] -> Nothing
    ws' ->
      Just $
        header "A task succeeded with warnings"
          <> numbered "Warning" (map renderEmittedWarn ws')
  where
    notIgnoredErrs :: [EmittedWarn]
    notIgnoredErrs = filter (not . isIgnoredErrGroup) ws

    isIgnoredErrGroup :: EmittedWarn -> Bool
    isIgnoredErrGroup IgnoredErrGroup{} = True
    isIgnoredErrGroup _ = False

renderEmittedWarn :: EmittedWarn -> Doc AnsiStyle
renderEmittedWarn (IgnoredErrGroup ectx es) = renderedCtx <> renderedErrors
  where
    renderedCtx =
      case ectx of
        [] -> emptyDoc
        _ ->
          (vsep (map (\ctx -> renderErrCtx ctx <> line) ectx))

    renderedErrors =
      section
        "Relevant errors"
        $ numbered "Error" (map renderErrWithStack (NE.toList es))
renderEmittedWarn (StandaloneWarn (SomeWarn warn)) = renderDiagnostic warn
renderEmittedWarn (WarnOnErrGroup ws ectx es) = renderedWarnings <> renderedCtx <> renderedErrors
  where
    renderedWarnings = vsep (map (\w -> renderSomeWarn w <> line) (NE.toList ws)) <> line

    renderedCtx =
      case ectx of
        [] -> emptyDoc
        _ ->
          section
            "Additional context"
            (vsep (map (\ctx -> renderErrCtx ctx <> line) ectx))

    renderedErrors =
      section
        "Relevant errors"
        $ numbered "Error" (map renderErrWithStack (NE.toList es))

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
  annotate (color Yellow) (">>> " <> name) <> line <> line
    <> indent 2 content
    <> line

numbered :: Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
numbered name = vsep . zipWith (\n single -> title n <> line <> line <> indent 2 single <> line) [1 ..]
  where
    title n = annotate (color Yellow) (name <> " " <> viaShow @Int n)
