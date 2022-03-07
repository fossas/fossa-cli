{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.PrettyPrinter.Markdown (
  SimpleMarkdown (),
  bold,
  italics,
  h1,
  h2,
  codeBlock,
  codeInline,
  renderMarkdown,
  renderAnsiToMarkdown,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextLazy
import Data.Text.Lazy.Builder qualified as TextLazyBuilder
import Prettyprinter (
  Doc,
  SimpleDocStream (..),
  annotate,
 )
import Prettyprinter.Internal (textSpaces)
import Prettyprinter.Render.Terminal.Internal (
  AnsiStyle (SetAnsiStyle),
 )
import Prettyprinter.Render.Util.Panic (
  panicInputNotFullyConsumed,
  panicPeekedEmpty,
  panicPoppedEmpty,
  panicUncaughtFail,
  panicUnpairedPop,
 )

-- | Simplified Markdown.
--
-- Supports following style:
--  * heading
--  * bold
--  * italics
--  * code (both in block, and inline)
--
-- Reference: https://www.markdownguide.org/basic-syntax.
data SimpleMarkdown
  = H1
  | H2
  | Bold
  | Italics
  | CodeBlock
  | CodeInline
  deriving (Eq, Ord, Show)

bold :: Doc SimpleMarkdown -> Doc SimpleMarkdown
bold = annotate Bold

italics :: Doc SimpleMarkdown -> Doc SimpleMarkdown
italics = annotate Italics

h1 :: Doc SimpleMarkdown -> Doc SimpleMarkdown
h1 = annotate H1

h2 :: Doc SimpleMarkdown -> Doc SimpleMarkdown
h2 = annotate H2

codeBlock :: Doc SimpleMarkdown -> Doc SimpleMarkdown
codeBlock = annotate CodeBlock

codeInline :: Doc SimpleMarkdown -> Doc SimpleMarkdown
codeInline = annotate CodeInline

-- | @('renderMarkdown' sdoc)@ takes the output @sdoc@ and gives strict text.
--
-- Example:
--
-- @
-- let doc = (h1 "Foo") <> (h2 "bar" <+> codeInline "baz")
-- let sdoc = layoutPretty defaultLayoutOptions doc
-- renderMarkdown sdoc
--
-- # Foo
-- ## bar `baz`
-- @
--
-- For simplicity it intentionally renders line break
-- after heading and code block.
--
-- Reference: https://www.markdownguide.org/basic-syntax.
renderMarkdown :: SimpleDocStream SimpleMarkdown -> Text
renderMarkdown = go []
  where
    go :: [SimpleMarkdown] -> SimpleDocStream SimpleMarkdown -> Text
    go _ SFail = panicUncaughtFail
    go [] SEmpty = mempty
    go (_ : _) SEmpty = panicInputNotFullyConsumed
    go stack (SChar c rest) = (Text.singleton c) <> go stack rest
    go stack (SText _l t rest) = t <> go stack rest
    go stack (SLine i rest) = (Text.singleton '\n') <> (textSpaces i) <> go stack rest
    go stack (SAnnPush ann rest) = startTag ann <> go (ann : stack) rest
    go (ann : stack) (SAnnPop rest) = endTag ann <> go stack rest
    go [] SAnnPop{} = panicUnpairedPop

    startTag :: SimpleMarkdown -> Text
    startTag tag = fst $ mkdownEnclosed tag

    endTag :: SimpleMarkdown -> Text
    endTag tag = snd $ mkdownEnclosed tag

    mkdownEnclosed :: SimpleMarkdown -> (Text, Text)
    mkdownEnclosed = \case
      Bold -> ("**", "**")
      Italics -> ("*", "*")
      H1 -> ("# ", "\n\n") -- Peeking in @SAnnPush@ and @SAnnPop@ can be used to embed @hardline@ appropriately,
      H2 -> ("## ", "\n\n")
      CodeBlock -> ("\n```\n", "\n```\n")
      CodeInline -> ("`", "`")

-- | @('renderAnsiToMarkdown' ddoc)@ takes the output @sdoc@ and
-- gives strict text formatted in markdown.
--
-- It only supports bold, and italics.
--
-- If document has color (text, foreground, background, etc.), and underline
-- styles, they will be ignored, and rendered as text without any annotation.
renderAnsiToMarkdown :: SimpleDocStream AnsiStyle -> Text
renderAnsiToMarkdown = TextLazy.toStrict . TextLazyBuilder.toLazyText . go [mempty]
  where
    go :: [AnsiStyle] -> SimpleDocStream AnsiStyle -> TextLazyBuilder.Builder
    go s sds = case sds of
      SFail -> panicUncaughtFail
      SEmpty -> mempty
      SChar c rest -> TextLazyBuilder.singleton c <> go s rest
      SText _ t rest -> TextLazyBuilder.fromText t <> go s rest
      SLine i rest -> TextLazyBuilder.singleton '\n' <> TextLazyBuilder.fromText (Text.replicate i " ") <> go s rest
      SAnnPush style rest -> TextLazyBuilder.fromText (toMarkdownText newStyle) <> go (push style s) rest
        where
          newStyle = style <> unsafePeek s
      SAnnPop rest -> TextLazyBuilder.fromText (toMarkdownText newStyle) <> go s' rest
        where
          s' = snd $ unsafePop s
          newStyle = unsafePeek s'

    push :: a -> [a] -> [a]
    push x = (x :)

    unsafePeek :: [a] -> a
    unsafePeek [] = panicPeekedEmpty
    unsafePeek (x : _) = x

    unsafePop :: [a] -> (a, [a])
    unsafePop [] = panicPoppedEmpty
    unsafePop (x : xs) = (x, xs)

    toMarkdownText :: AnsiStyle -> Text
    toMarkdownText (SetAnsiStyle _ _ b i _) = mdBolded b <> mdItalicized i

    mdBolded :: Maybe a -> Text
    mdBolded = maybe mempty (const "*")

    mdItalicized :: Maybe a -> Text
    mdItalicized = maybe mempty (const "_")
