module App.Fossa.Config.Container.Common (
  ImageText (..),
  imageTextArg,
  collectDockerHost,
  collectArch,
) where

import App.Fossa.Config.EnvironmentVars (EnvVars (EnvVars, envDockerHost))
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Options.Applicative (Parser, argument, help, metavar, str)
import System.Info (arch)

newtype ImageText = ImageText
  { unImageText :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ImageText where
  toEncoding = genericToEncoding defaultOptions

imageTextArg :: Parser ImageText
imageTextArg = ImageText <$> argument str (metavar "IMAGE" <> help "The image to scan")

-- | Get current runtime arch, We use this to find suitable image,
-- if multi-platform image is discovered. This is similar to
-- how docker pull, and existing behavior works
--
-- Ref: https://docs.docker.com/desktop/multi-arch/
collectArch :: Text
collectArch =
  toText $
    if arch == "x86_64" -- x86_64 is equivalent to amd64
      then "amd64"
      else arch

collectDockerHost :: EnvVars -> Text
collectDockerHost EnvVars{envDockerHost} =
  case envDockerHost of
    Nothing -> defaultDockerHost
    Just host ->
      if Text.isPrefixOf "unix://" host
        then withoutUnixSocketScheme host
        else defaultDockerHost
  where
    withoutUnixSocketScheme :: Text -> Text
    withoutUnixSocketScheme = Text.replace "unix://" ""

    defaultDockerHost :: Text
    defaultDockerHost = "/var/run/docker.sock"
