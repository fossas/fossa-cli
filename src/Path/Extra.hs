module Path.Extra (
  tryMakeRelative,
) where

import Path

-- tryMakeRelative returns the path of an absolute file (Path Abs File) relative to an absolute directory (Path Abs Dir).
-- If the file is not within the directory, then the absolute file path will be returned
tryMakeRelative :: Path Abs Dir -> Path Abs File -> SomeBase File
tryMakeRelative absDir absFile =
  case stripProperPrefix absDir absFile of
    Left _ -> Abs absFile
    Right relFile -> Rel relFile
