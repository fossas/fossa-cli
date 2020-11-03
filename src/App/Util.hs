{-# LANGUAGE DataKinds #-}

module App.Util
  ( validateDir,
  )
where

import App.Types
import Control.Monad (unless)
import qualified Path.IO as P
import System.Exit (die)

-- | Validate that a filepath points to a directory and the directory exists
validateDir :: FilePath -> IO BaseDir
validateDir dir = do
  absolute <- P.resolveDir' dir
  exists <- P.doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")
  pure $ BaseDir absolute
