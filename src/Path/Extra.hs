module Path.Extra (
  tryMakeRelative,
  renderRelative,
  extensionOf,
) where

import Data.String.Conversion (toText)
import Data.Text (Text)
import Path (Abs, Dir, File, Path, SomeBase (..), fileExtension, stripProperPrefix)

-- tryMakeRelative returns the path of an absolute file (Path Abs File) relative to an absolute directory (Path Abs Dir).
-- If the file is not within the directory, then the absolute file path will be returned
tryMakeRelative :: Path Abs Dir -> Path Abs File -> SomeBase File
tryMakeRelative absDir absFile =
  case stripProperPrefix absDir absFile of
    Left _ -> Abs absFile
    Right relFile -> Rel relFile

-- | Render the relative path between a Path Abs Dir and a Path Abs File that is supposed to be in that dir.
-- Intended for convenience when displaying the newly relative path; to interact with it use `tryMakeRelative` instead.
renderRelative :: Path Abs Dir -> Path Abs File -> Text
renderRelative absDir absFile = toText $ tryMakeRelative absDir absFile

extensionOf :: Path Abs File -> Maybe Text
extensionOf absFile = toText <$> fileExtension absFile
