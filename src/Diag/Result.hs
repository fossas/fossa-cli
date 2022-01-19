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

-- FIXME: considerations about ordering of warnings/errors
data Result a = Failure [EmittedWarn] ErrGroup | Success [EmittedWarn] a
  deriving (Show)

-- TODO: add UncaughtErrGroup constructor?
data EmittedWarn = StandaloneWarn SomeWarn | WarnOnErrGroup (NonEmpty SomeWarn) [ErrCtx] (NonEmpty ErrWithStack)
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

---------- Rendering

-- FIXME: should WarnOnErrGroup err groups get emitted in this message somewhere?
renderFailure :: [EmittedWarn] -> ErrGroup -> Doc AnsiStyle
renderFailure _ (ErrGroup _ ectx es) = header <> renderedCtx <> renderedErrs
  where
    header =
      annotate (color Yellow) "----------"
        <> line
        <> annotate (color Yellow) "An issue occurred"
        <> line
        <> line
    renderedCtx =
      case ectx of
        [] -> emptyDoc
        _ ->
          annotate (color Yellow) ">>> Description" <> line <> line
            <> indent 2 (vsep (map (\ctx -> renderErrCtx ctx <> line) ectx))
            <> line
    renderedErrs =
      annotate (color Yellow) ">>> Relevant errors" <> line <> line
        <> indent 2 (vsep (map (\err -> annotate (color Yellow) "---" <> line <> renderErrWithStack err <> line) (NE.toList es)))

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

renderSuccess :: [EmittedWarn] -> Doc AnsiStyle
renderSuccess ws = header <> renderedWarnings
  where
    header =
      annotate (color Yellow) "----------"
        <> line
        <> annotate (color Yellow) "A task succeeded with warnings"
        <> line
        <> line

    renderedWarnings =
      (vsep (map (\w -> annotate (color Yellow) "----- Warning" <> line <> line <> renderEmittedWarn w <> line) ws))

renderEmittedWarn :: EmittedWarn -> Doc AnsiStyle
renderEmittedWarn (StandaloneWarn (SomeWarn warn)) = renderDiagnostic warn
renderEmittedWarn (WarnOnErrGroup ws ectx es) = renderedWarnings <> renderedCtx <> renderedErrors
  where
    renderedWarnings = vsep (map (\w -> renderSomeWarn w <> line) (NE.toList ws)) <> line

    renderedCtx =
      case ectx of
        [] -> emptyDoc
        _ ->
          annotate (color Yellow) ">>> Additional context" <> line <> line
            <> indent 2 (vsep (map (\ctx -> renderErrCtx ctx <> line) ectx))
            <> line

    renderedErrors =
      annotate (color Yellow) ">>> Relevant errors" <> line <> line
        <> indent 2 (vsep (map (\err -> annotate (color Yellow) "---" <> line <> renderErrWithStack err <> line) (NE.toList es)))

renderSomeWarn :: SomeWarn -> Doc AnsiStyle
renderSomeWarn (SomeWarn w) = renderDiagnostic w
