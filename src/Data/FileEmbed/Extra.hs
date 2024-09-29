module Data.FileEmbed.Extra (
  embedFile',
  embedFileIfExists,
)
where

import Control.Exception (try)
import Control.Monad (when)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import Language.Haskell.TH (reportWarning, runIO)
import Language.Haskell.TH.Syntax (Exp (LitE), Lit (..), Q)
import Path (parseAbsFile)
import Path.IO (doesFileExist)
import System.Directory (getCurrentDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (isRelative, splitPath, takeDirectory)

embedFileIfExists :: FilePath -> Q Exp
embedFileIfExists inputPath = do
  skipEmbedEnvVar <- runIO $ lookupEnv "FOSSA_SKIP_EMBED_FILE_IN_HLS"
  -- During some commands (like cabal install) the working directory may not
  -- be the same as the root of this project.
  -- makeRelativeToProject takes a relative path and roots it in the highest directory
  -- a .cabal file is found and returns an absolute path.
  inputPath' <- makeRelativeToProject inputPath
  case (skipEmbedEnvVar, parseAbsFile inputPath') of
    (Just _, _) -> do
      pure (LitE $ StringL "")
    (_, Just path) -> do
      exists <- doesFileExist path
      if exists
        then embedFile' inputPath
        else do
          reportWarning $ "File " <> inputPath <> " not found"
          pure (LitE $ StringL "")
    (_, Nothing) -> fail "No filepath provided"

-- | Like `embedFile`, but prints out debug logging when
-- @FOSSA_DEBUG_EMBED_FILE@ is set.
embedFile' :: FilePath -> Q Exp
embedFile' fp = runIO logEmbedFile *> embedFile fp
  where
    logEmbedFile :: IO (Either IOError ())
    logEmbedFile = do
      embedLogEnvVar <- lookupEnv "FOSSA_DEBUG_EMBED_FILE"
      try $
        when (isJust embedLogEnvVar) $ do
          cwd <- getCurrentDirectory
          putStrLn $ "CWD: " <> cwd

          putStrLn $ "Embedding file: " <> fp
          let isRel = isRelative fp
          putStrLn $ "Relative?: " <> show isRel

          let dirs = take (length (splitPath fp) - if isRel then 0 else 1) $ drop 1 $ iterate takeDirectory fp
          traverse_ ls dirs

    ls :: FilePath -> IO ()
    ls dir = do
      files <- listDirectory dir
      putStrLn $ "ls " <> show dir <> ": " <> show files
