module Path.Extra (
  tryMakeRelative,
  renderRelative,
  extensionOf,
  isChildOf,
) where

import Data.String.Conversion (toText)
import Data.Text (Text)
import Path (Abs, Dir, File, Path, SomeBase (..), fileExtension, stripProperPrefix)

-- tryMakeRelative returns the path of an absolute file (Path Abs File) relative to an absolute directory (Path Abs Dir).
-- If the file is not within the directory, then the absolute file path will be returned
tryMakeRelative :: Path Abs Dir -> Path Abs t -> SomeBase t
tryMakeRelative absDir absFile =
  case stripProperPrefix absDir absFile of
    Left _ -> Abs absFile
    Right relFile -> Rel relFile

-- | Render the relative path between a Path Abs Dir and a Path Abs File that is supposed to be in that dir.
-- Intended for convenience when displaying the newly relative path; to interact with it use `tryMakeRelative` instead.
renderRelative :: Path Abs Dir -> Path Abs t -> Text
renderRelative absDir absFile = toText $ tryMakeRelative absDir absFile

extensionOf :: Path Abs File -> Maybe Text
extensionOf absFile = toText <$> fileExtension absFile

isChildOf :: Path Abs t -> Path Abs Dir -> Bool
isChildOf target root = case tryMakeRelative root target of
  Abs _ -> False
  _ -> True
