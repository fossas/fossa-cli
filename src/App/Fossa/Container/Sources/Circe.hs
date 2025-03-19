module App.Fossa.Container.Sources.Circe (
  circeReexportCommand,
)
where

import App.Fossa.Config.Container.Common (ImageText, unImageText)
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Effect.Exec (AllowErr (..), Command (..))

-- | Build the command to run circe reexport with a raw image reference
circeReexportCommand ::
  BinaryPaths ->
  ImageText ->
  Text ->
  Command
circeReexportCommand paths img outputPath =
  Command
    { cmdName = toText $ toPath paths
    , cmdArgs = ["reexport"] <> ref <> [outputPath]
    , cmdAllowErr = Never
    }
  where
    ref = [unImageText img]
