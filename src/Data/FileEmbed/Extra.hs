module Data.FileEmbed.Extra (
  embedFileIfExists,
) where

import Data.FileEmbed (embedFile)
import Language.Haskell.TH (Exp (LitE), Lit (StringL), Q, reportWarning, runIO)
import Path (parseRelFile)
import Path.IO (doesFileExist)
import System.Environment (lookupEnv)

embedFileIfExists :: FilePath -> Q Exp
embedFileIfExists inputPath = do
  skipEmbedEnvVar <- runIO $ lookupEnv "FOSSA_SKIP_EMBED_FILE_IN_HLS"
  case (skipEmbedEnvVar, parseRelFile inputPath) of
    (Just _, _) -> do
      pure (LitE $ StringL "")
    (_, Just path) -> do
      exists <- doesFileExist path
      if exists
        then embedFile inputPath
        else do
          reportWarning $ "File " <> inputPath <> " not found"
          pure (LitE $ StringL "")
    (_, Nothing) -> fail "No filepath provided"
