module App.Fossa.Container.Sources.Circe (
  circeAuthArgs,
  circeReexportCommand,
)
where

import App.Fossa.EmbeddedBinary (BinaryPaths, toPath)
import Container.Docker.SourceParser (RegistryImageSource (..), registryCred, toCirceReference)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Effect.Exec (AllowErr (..), Command (..))

-- | Build the command to run circe reexport
circeReexportCommand ::
  BinaryPaths ->
  RegistryImageSource ->
  Text ->
  Command
circeReexportCommand paths imgSrc outputPath =
  Command
    { cmdName = toText $ toPath paths
    , cmdArgs = ["reexport"] <> ref <> [outputPath] <> auth
    , cmdAllowErr = Never
    }
  where
    ref = [toCirceReference imgSrc]
    auth = circeAuthArgs imgSrc

-- | Extract authentication arguments for circe commands
circeAuthArgs :: RegistryImageSource -> [Text]
circeAuthArgs img = case registryCred img of
  Just (username, password) ->
    [ "--username"
    , username
    , "--password"
    , password
    ]
  Nothing -> []
