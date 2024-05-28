module App.Fossa.Config.SBOM.Common (
  SBOMFile (..),
  sbomFileArg,
  getProjectRevision,
) where

import App.Fossa.Config.Common (CacheAction (..))
import App.Fossa.ProjectInference (InferredProject (..), inferProjectDefaultFromFile, readCachedRevision, saveRevision)
import App.Types (OverrideProject (..), ProjectRevision (..))
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (fromEitherShow, (<||>))
import Control.Effect.Diagnostics (context)
import Control.Effect.Diagnostics qualified as Diag
import Control.Effect.Lift (Lift)
import Control.Monad (when)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (toString)
import Data.Text (Text)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Options.Applicative (Parser, argument, metavar, str)
import Path (parseSomeFile)
import Path.Posix (SomeBase (..))
import Style (applyFossaStyle, stringToHelpDoc)

newtype SBOMFile = SBOMFile
  { unSBOMFile :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SBOMFile where
  toEncoding = genericToEncoding defaultOptions

sbomFileArg :: Parser SBOMFile
sbomFileArg = SBOMFile <$> argument str (applyFossaStyle <> metavar "SBOM" <> stringToHelpDoc "Path to the SBOM file to scan")

getProjectRevision ::
  ( Has Diag.Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  SBOMFile ->
  OverrideProject ->
  CacheAction ->
  m ProjectRevision
getProjectRevision sbomPath override cacheStrategy = do
  let path = unSBOMFile $ sbomPath
  parsedPath <- context "Parsing `sbom` path" $ fromEitherShow $ parseSomeFile (toString path)
  inferred <- case parsedPath of
    Abs f -> inferProjectDefaultFromFile f
    Rel f -> inferProjectDefaultFromFile f

  inferredVersion <- case cacheStrategy of
    ReadOnly -> do
      readCachedRevision <||> pure (inferredRevision inferred)
    WriteOnly -> do
      pure $ inferredRevision inferred
  let name = fromMaybe (inferredName inferred) $ overrideName override
  let version = fromMaybe inferredVersion $ overrideRevision override
  let revision = ProjectRevision name version Nothing
  when (cacheStrategy == WriteOnly) $ saveRevision revision
  pure $ ProjectRevision name version Nothing
