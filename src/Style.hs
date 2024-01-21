module Style (coloredText, formatStringToDoc, formatDoc, applyFossaStyle, boldItalicized, coloredBoldItalicized, coloredBoldItalicizedString) where

import Data.String.Conversion (toString)
import Effect.Logger (newlineTrailing, vsep)
import Options.Applicative (style)
import Options.Applicative.Builder (Mod)
import Prettyprinter (Doc, annotate, defaultLayoutOptions, indent, layoutPretty, pretty)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Green), bold, color, italicized, renderStrict)

formatStringToDoc :: String -> Maybe (Doc AnsiStyle)
formatStringToDoc s = Just $ indent 2 $ vsep [newlineTrailing $ pretty s]

formatDoc :: Doc AnsiStyle -> Doc AnsiStyle
formatDoc doc = indent 2 $ newlineTrailing doc

coloredText :: Color -> Doc AnsiStyle -> String
coloredText clr str = toString . renderStrict . layoutPretty defaultLayoutOptions $ annotate (color clr) str

coloredBoldItalicizedString :: Color -> Doc AnsiStyle -> String
coloredBoldItalicizedString clr str = toString . renderStrict . layoutPretty defaultLayoutOptions $ annotate (color clr <> bold <> italicized) str

applyFossaStyle :: Mod f a
applyFossaStyle = style $ annotate (color Green <> bold <> italicized)

boldItalicized :: Doc AnsiStyle -> Doc AnsiStyle
boldItalicized = annotate (bold <> italicized)

coloredBoldItalicized :: Color -> Doc AnsiStyle -> Doc AnsiStyle
coloredBoldItalicized clr = annotate (color clr <> bold <> italicized)
