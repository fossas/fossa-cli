module Path.Extra (
  tryMakeRelative,
  renderRelative,
  resolveAbsolute,
  extensionOf,
  isChildOf,
  SomePath (..),
  SomeResolvedPath (..),
) where

import Control.Effect.Record (RecordableValue)
import Control.Effect.Replay (ReplayableValue)
import Data.Aeson (FromJSON, ToJSON)
import Data.String.Conversion (ToText, toText)
import Data.Text (Text)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, SomeBase (..), fileExtension, stripProperPrefix, (</>))

-- | Path of some type, can be any combination of @Path (Abs | Rel) (File | Dir)@.
-- Pattern match to find the variant expressed.
data SomePath
  = SomeFile (SomeBase File)
  | SomeDir (SomeBase Dir)
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SomePath
instance RecordableValue SomePath
instance FromJSON SomePath
instance ReplayableValue SomePath

instance ToText SomePath where
  toText (SomeFile f) = toText f
  toText (SomeDir d) = toText d

-- | Returns the path of an absolute file (Path Abs File) relative to an absolute directory (Path Abs Dir).
-- If the file is not within the directory, then the absolute file path will be returned
tryMakeRelative :: Path Abs Dir -> Path Abs t -> SomeBase t
tryMakeRelative absDir absFile =
  case stripProperPrefix absDir absFile of
    Left _ -> Abs absFile
    Right relFile -> Rel relFile

-- | A resolved path, can be a directory or a file but is always absolute.
-- Pattern match to find the variant expressed.
data SomeResolvedPath
  = ResolvedDir (Path Abs Dir)
  | ResolvedFile (Path Abs File)
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SomeResolvedPath
instance RecordableValue SomeResolvedPath
instance FromJSON SomeResolvedPath
instance ReplayableValue SomeResolvedPath

instance ToText SomeResolvedPath where
  toText (ResolvedFile f) = toText f
  toText (ResolvedDir d) = toText d

-- | Resolves a potentially relative path to an absolute path, given a path to use as a base.
-- If the path provided is already absolute, it is returned without modification.
-- Often the correct base to use is the current working directory.
resolveAbsolute :: Path Abs Dir -> SomePath -> SomeResolvedPath
resolveAbsolute _ (SomeFile (Abs file)) = ResolvedFile file
resolveAbsolute base (SomeFile (Rel file)) = ResolvedFile $ base </> file
resolveAbsolute _ (SomeDir (Abs dir)) = ResolvedDir dir
resolveAbsolute base (SomeDir (Rel dir)) = ResolvedDir $ base </> dir

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
