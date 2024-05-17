module App.Fossa.Config.SBOM.Common (
  SBOMFile (..),
  sbomFileArg,
) where

import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Text (Text)
import GHC.Generics (Generic)
import Options.Applicative (Parser, argument, metavar, str)
import Style (applyFossaStyle, stringToHelpDoc)

newtype SBOMFile = SBOMFile
  { unSBOMFile :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SBOMFile where
  toEncoding = genericToEncoding defaultOptions

sbomFileArg :: Parser SBOMFile
sbomFileArg = SBOMFile <$> argument str (applyFossaStyle <> metavar "SBOM" <> stringToHelpDoc "The SBOM file to scan")
