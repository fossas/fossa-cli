module App.Fossa.Config.Container.Common (
  ImageText (..),
  imageTextArg,
  collectDockerHost,
  collectArch,
) where

import App.Fossa.Config.EnvironmentVars (EnvVars (EnvVars, envDockerHost))
import Control.Effect.Diagnostics (Diagnostics, Has, ToDiagnostic, renderDiagnostic, warn)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Effect.Logger (renderIt)
import Errata (Errata (..))
import GHC.Generics (Generic)
import Options.Applicative (Parser, argument, metavar, str)
import Prettyprinter (pretty, vsep)
import Style (applyFossaStyle, stringToHelpDoc)
import System.Info (arch)

newtype ImageText = ImageText
  { unImageText :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ImageText where
  toEncoding = genericToEncoding defaultOptions

imageTextArg :: Parser ImageText
imageTextArg = ImageText <$> argument str (applyFossaStyle <> metavar "IMAGE" <> stringToHelpDoc "The image to scan")

-- | Get current runtime arch, We use this to find suitable image,
-- if multi-platform image is discovered. This is similar to
-- how docker pull, and existing behavior works
--
-- Ref: https://docs.docker.com/desktop/multi-arch/
collectArch :: Text
collectArch =
  toText $
    case arch of
      ("x86_64") -> "amd64" -- x86_64 is equivalent to amd64
      ("aarch64") -> "arm64" -- aarch64 is equivalent to arm64
      other -> other

collectDockerHost :: Has Diagnostics sig m => EnvVars -> m Text
collectDockerHost EnvVars{envDockerHost} =
  case envDockerHost of
    Nothing -> pure defaultDockerHost
    Just host ->
      if Text.isPrefixOf "unix://" host
        then pure $ withoutUnixSocketScheme host
        else do
          warn $ NotSupportedHostScheme host
          pure defaultDockerHost
  where
    withoutUnixSocketScheme :: Text -> Text
    withoutUnixSocketScheme = Text.replace "unix://" ""

defaultDockerHost :: Text
defaultDockerHost = "/var/run/docker.sock"

newtype NotSupportedHostScheme = NotSupportedHostScheme Text

instance ToDiagnostic NotSupportedHostScheme where
  renderDiagnostic (NotSupportedHostScheme provided) = do
    let header = "Host scheme not supported"
        body =
          renderIt $
            vsep
              [ "Only unix domain sockets are supported for DOCKER_HOST value."
              , pretty $ "fossa will use: " <> "unix://" <> defaultDockerHost <> " instead, to connect with docker engine api (if needed)."
              , "Provided 'DOCKER_HOST' via environment variable: " <> pretty provided
              ]
    Errata (Just header) [] (Just body)
