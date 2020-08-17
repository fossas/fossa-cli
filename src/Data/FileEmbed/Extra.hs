module Data.FileEmbed.Extra 
  ( embedFileIfExists
  ) where

import Prelude
import Path
import Path.IO
import Language.Haskell.TH
import Data.FileEmbed (embedFile)

embedFileIfExists :: FilePath -> Q Exp
embedFileIfExists inputPath = do
  case (parseRelFile inputPath) of
    (Just path) -> do
      exists <- doesFileExist path
      case exists of
        True -> embedFile inputPath
        False -> do
          reportWarning $ "File " <> inputPath <> " not found"
          pure (LitE $ StringL "")
    Nothing -> fail "No filepath provided"
