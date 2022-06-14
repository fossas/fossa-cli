{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.FileEmbed.Extra (
  embedFileL,
  embedFileIfExists,
) where

import Control.Exception (try)
import Control.Monad (when)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Internal qualified as B
import Data.ByteString.Unsafe (unsafePackAddressLen)
import Data.FileEmbed (embedFile)
import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import Language.Haskell.TH (bytesPrimL, mkBytes, reportWarning, runIO)
import Language.Haskell.TH.Syntax (
  Exp (AppE, LitE, VarE),
  Lit (..),
  Q,
  qAddDependentFile,
 )
import Path (parseRelFile)
import Path.IO (doesFileExist)
import System.Directory (getCurrentDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (isRelative, splitPath, takeDirectory)
import System.IO.Unsafe (unsafePerformIO)

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

-- | Like `embedFile`, but prints out debug logging when
-- @FOSSA_DEBUG_EMBED_FILE@ is set.
embedFileL :: FilePath -> Q Exp
embedFileL fp = qAddDependentFile fp >> runIO embedWithLogging >>= bsToExp
  where
    embedWithLogging = do
      embedLogEnvVar <- lookupEnv "FOSSA_DEBUG_EMBED_FILE"
      _ :: Either IOError () <- try $
        when (isJust embedLogEnvVar) $ do
          cwd <- getCurrentDirectory
          putStrLn $ "CWD: " <> cwd

          putStrLn $ "Embedding file: " <> fp
          let isRel = isRelative fp
          putStrLn $ "Relative?: " <> show isRel

          let dirs = take (length (splitPath fp) - if isRel then 0 else 1) $ tail $ iterate takeDirectory fp
          traverse_ ls dirs
      B.readFile fp

    ls :: FilePath -> IO ()
    ls dir = do
      files <- listDirectory dir
      putStrLn $ "ls " <> show dir <> ": " <> show files

-- See https://github.com/snoyberg/file-embed/blob/548430d2a79bb6f4cb4256768761071f59909aa5/Data/FileEmbed.hs#L184-L204
bsToExp :: B.ByteString -> Q Exp
bsToExp bs =
  pure $
    VarE 'unsafePerformIO
      `AppE` ( VarE 'unsafePackAddressLen
                `AppE` LitE (IntegerL $ fromIntegral $ B8.length bs)
                `AppE` LitE
                  ( bytesPrimL
                      ( let B.PS ptr off sz = bs
                         in mkBytes ptr (fromIntegral off) (fromIntegral sz)
                      )
                  )
             )
