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
  ErrHelp (..),
  ErrSupport (..),
  ErrDoc (..),

  -- * Helpers
  resultToMaybe,

  -- * Rendering
  renderFailure,
  renderSuccess,
  renderFailureWithoutWarnings,
) where

import Data.Error (DiagnosticStyle (..), applyDiagnosticStyle, combineErrataHeaders, renderErrataStack)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
import Effect.Logger (newlinePreceding, newlineTrailing)
import Errata (Errata)
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
  | WarnOnErrGroup (NonEmpty SomeWarn) [ErrCtx] [ErrHelp] [ErrSupport] [ErrDoc] (NonEmpty ErrWithStack)
  | IgnoredErrGroup [ErrCtx] [ErrHelp] [ErrSupport] [ErrDoc] (NonEmpty ErrWithStack)
  deriving (Show)

-- | An error, or group of errors, that occurred during a computation that led
-- to a Failure. An ErrGroup can have warnings and error context attached.
data ErrGroup = ErrGroup [SomeWarn] [ErrCtx] [ErrHelp] [ErrSupport] [ErrDoc] (NonEmpty ErrWithStack)
  deriving (Show)

instance Semigroup ErrGroup where
  (<>) :: ErrGroup -> ErrGroup -> ErrGroup
  ErrGroup sws ectx ehlp esup edoc nee <> ErrGroup sws' ectx' ehlp' esup' edoc' nee' = ErrGroup (sws <> sws') (ectx <> ectx') (ehlp <> ehlp') (esup <> esup') (edoc <> edoc') (nee <> nee')

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

-- | Some error help type. Right now, this just requries a ToDiagnostics instance for the type.
data ErrHelp where
  ErrHelp :: ToDiagnostic diag => diag -> ErrHelp

instance Show ErrHelp where
  showsPrec p (ErrHelp h) = showParen (p > 10) $ showString "ErrHelp " . diagToShowS h

-- | Some error support type. Right now, this just requries a ToDiagnostics instance for the type.
data ErrSupport where
  ErrSupport :: ToDiagnostic diag => diag -> ErrSupport

instance Show ErrSupport where
  showsPrec p (ErrSupport s) = showParen (p > 10) $ showString "ErrSupport " . diagToShowS s

-- | Some error doc type. Right now, this just requries a ToDiagnostics instance for the type.
data ErrDoc where
  ErrDoc :: ToDiagnostic diag => diag -> ErrDoc

instance Show ErrDoc where
  showsPrec p (ErrDoc d) = showParen (p > 10) $ showString "ErrDoc " . diagToShowS d

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
renderFailure ws (ErrGroup _ ectx ehlp esup edoc es) headerDoc = do
  let errDetails = combineErrDetails edoc esup ehlp ectx
      renderedErrsWithAnnotation = section "Relevant Errors" $ unannotatedSubsection $ renderErrs es errDetails Default ErrorStyle
  header headerDoc <> renderedErrsWithAnnotation <> renderedPossibleErrs
  where
    renderedPossibleErrs :: Doc AnsiStyle
    renderedPossibleErrs =
      case ws of
        [] -> emptyDoc
        _ ->
          section "Possibly Related Warnings" $
            unannotatedSubsection (map (renderEmittedWarn Default) ws)

renderFailureWithoutWarnings :: ErrGroup -> Doc AnsiStyle -> Doc AnsiStyle
renderFailureWithoutWarnings (ErrGroup _ ectx ehlp esup edoc es) headerDoc = do
  let errDetails = combineErrDetails edoc esup ehlp ectx
      renderedErrs = section "Relevant Errors" $ unannotatedSubsection $ renderErrs es errDetails None ErrorStyle
  header headerDoc <> renderedErrs

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

---------- Rendering a collection of Result components: [ErrCtx], [ErrHelp], [ErrSupport], [ErrDoc], NonEmpty ErrWithStack

renderErrCtxStack :: [ErrCtx] -> Doc AnsiStyle
renderErrCtxStack eCtxStack = case eCtxStack of
  [] -> emptyDoc
  _ -> renderErrataStack [combineErrataHeaders (map ((applyDiagnosticStyle ContextStyle) . renderErrCtx) eCtxStack)]
  where
    renderErrCtx :: ErrCtx -> Errata
    renderErrCtx (ErrCtx c) = renderDiagnostic c

renderErrHelpStack :: [ErrHelp] -> Doc AnsiStyle
renderErrHelpStack eHelpStack = case eHelpStack of
  [] -> emptyDoc
  _ -> newlineTrailing $ renderErrataStack [combineErrataHeaders (map ((applyDiagnosticStyle HelpStyle) . renderErrHelp) eHelpStack)]
  where
    renderErrHelp :: ErrHelp -> Errata
    renderErrHelp (ErrHelp h) = renderDiagnostic h

renderErrSupportStack :: [ErrSupport] -> Doc AnsiStyle
renderErrSupportStack eSuppStack = case eSuppStack of
  [] -> emptyDoc
  _ -> newlineTrailing $ renderErrataStack [combineErrataHeaders (map ((applyDiagnosticStyle SupportStyle) . renderErrSupport) eSuppStack)]
  where
    renderErrSupport :: ErrSupport -> Errata
    renderErrSupport (ErrSupport s) = renderDiagnostic s

renderErrDocStack :: [ErrDoc] -> Doc AnsiStyle
renderErrDocStack eDocStack = case eDocStack of
  [] -> emptyDoc
  _ -> newlineTrailing $ renderErrataStack [combineErrataHeaders $ map ((applyDiagnosticStyle DocumentationStyle) . renderErrDoc) eDocStack]
  where
    renderErrDoc :: ErrDoc -> Errata
    renderErrDoc (ErrDoc d) = renderDiagnostic d

combineErrDetails :: [ErrDoc] -> [ErrSupport] -> [ErrHelp] -> [ErrCtx] -> Doc AnsiStyle
combineErrDetails edoc esupp ehelp ectx = renderErrDocStack edoc <> renderErrSupportStack esupp <> renderErrHelpStack ehelp <> renderErrCtxStack ectx

-- renderErrs args:
--  * NonEmpty ErrWithStack which has the same type as [ErrWithStack]
--      ErrWithStack is an error and its associated traceback stack.
--  * DocAnsiStyle
--      This will be the rendered errDetails that have been gathered from [ErrDoc], [ErrSupport], [ErrHelp], and [ErrCtx].
--  * TracebackStyle
--      Used to determine whether we want to show traceback messages. Currently, tracebacks are only shown in --debug mode.
--  * DiagnosticStyle
--      Used to determine what type of DiagnosticStyle we want to display. In the context of this function it will either be WarningStyle or ErrorStyle.
--      renderErrs is used by ErrGroup and EmittedWarn. The EmittedWarn type can contain errors that have been recovered and converted into a warning through our warnOnErr effect
--      DiagnosticStyle allows us to tag these 'errors' accordingly (Warn or Error) depending on our entry point
--
-- renderErrs workflow:
--  * render our errors and their tracebacks as [ (Doc AnsiStyle, Doc AnsiStyle) ]
--  * add our errDetails to the first error in the list
--  * iterate through our list and combine the rendered err and rendered traceback
renderErrs :: NonEmpty ErrWithStack -> Doc AnsiStyle -> TracebackStyle -> DiagnosticStyle -> [Doc AnsiStyle]
renderErrs es errDetails tracebackStyle diagStyle = do
  let errsWithTraceback = map (\x -> renderErrWithStack x tracebackStyle diagStyle) (NE.toList es)
      errsWithTracebackAndErrDetails = applyToTopOfStack addErrDetails errsWithTraceback
  map (uncurry (<>)) errsWithTracebackAndErrDetails
  where
    applyToTopOfStack :: (a -> a) -> [a] -> [a]
    applyToTopOfStack _ [] = []
    applyToTopOfStack f (x : xs) = f x : xs

    addErrDetails :: (Doc AnsiStyle, Doc AnsiStyle) -> (Doc AnsiStyle, Doc AnsiStyle)
    addErrDetails (err, traceback) = (err <> (newlinePreceding . newlineTrailing $ errDetails), traceback)

---------- Rendering individual Result components: ErrCtx, EmittedWarn, SomeWarn, ErrWithStack

-- renderErrWithStack returns a tuple, where the first element in the tuple is the renderedErr, and the second element in the tuple is the err's rendered traceback
-- Returning this as a tuple to give us more control on how we want to construct our error messages
-- See `renderErrs` for full error message construction
renderErrWithStack :: ErrWithStack -> TracebackStyle -> DiagnosticStyle -> (Doc AnsiStyle, Doc AnsiStyle)
renderErrWithStack (ErrWithStack (Stack stack) (SomeErr err)) tracebackStyle diagStyle = do
  let traceback = case tracebackStyle of
        Default ->
          annotate (color Cyan) "Traceback:"
            <> line
            <> case stack of
              [] -> indent 2 "(none)"
              _ -> indent 2 (vsep (map (pretty . ("- " <>)) stack))
        None -> ""

      singleErr = renderErrataStack [applyDiagnosticStyle diagStyle $ renderDiagnostic err]

  (singleErr, traceback)

renderEmittedWarn :: TracebackStyle -> EmittedWarn -> Doc AnsiStyle
renderEmittedWarn tracebackStyle (IgnoredErrGroup ectx ehlp esup edoc es) = do
  let errDetails = combineErrDetails edoc esup ehlp ectx
  unannotatedSubsection $ renderErrs es errDetails tracebackStyle WarningStyle
renderEmittedWarn _ (StandaloneWarn warn) = renderErrataStack [renderSomeWarn warn]
renderEmittedWarn tracebackStyle (WarnOnErrGroup ws ectx ehlp esup edoc es) = do
  let errDetails = combineErrDetails edoc esup ehlp ectx
      renderedErrors = unannotatedSubsection $ renderErrs es errDetails tracebackStyle WarningStyle
      renderedWarnings = renderErrataStack (map renderSomeWarn (NE.toList ws))
  newlineTrailing renderedWarnings <> newlinePreceding renderedErrors

renderSomeWarn :: SomeWarn -> Errata
renderSomeWarn (SomeWarn w) = applyDiagnosticStyle WarningStyle $ renderDiagnostic w

---------- Rendering helpers

header :: Doc AnsiStyle -> Doc AnsiStyle
header name =
  annotate (color Yellow) name
    <> line
    <> line

section :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
section name content =
  annotate (color Yellow) ("*** " <> name <> " ***")
    <> line
    <> line
    <> indent 2 content
    <> line

unannotatedSubsection :: [Doc AnsiStyle] -> Doc AnsiStyle
unannotatedSubsection = vsep . map (indent 2)
