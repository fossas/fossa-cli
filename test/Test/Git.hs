{-# LANGUAGE QuasiQuotes #-}

-- | Minimal git checkouts for tests that read a checkout's git metadata.
module Test.Git (writeGitConfig) where

import Data.ByteString qualified as BS
import Data.String.Conversion (encodeUtf8)
import Data.Text (Text)
import Path (Abs, Dir, Path, reldir, relfile, toFilePath, (</>))
import Path.IO (createDirIfMissing)

-- | Makes the directory a git checkout whose @.git/config@ has the given
-- contents. Nothing else is written, so the checkout has no HEAD.
writeGitConfig :: Path Abs Dir -> Text -> IO ()
writeGitConfig checkout config = do
  let gitDir = checkout </> [reldir|.git|]
  createDirIfMissing True gitDir
  BS.writeFile (toFilePath $ gitDir </> [relfile|config|]) (encodeUtf8 config)
