
{-# language TemplateHaskell #-}

module Effect.ReadFS
  ( ReadFS(..)
  , readFSToIO
  , readFSVirtual

  , readContents
  , doesFileExist
  , doesDirExist
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Map (Map)
import           Data.Text (Text)
import           Path (Dir, File, Path, toFilePath)
import qualified Path.IO as PIO
import           Polysemy

{-
data WalkFS m a where
  WalkDir :: (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m (PIO.WalkAction Rel)) -> Path b Dir -> WalkFS m ()

makeSem ''WalkFS

blah :: Member (Embed IO) r => Sem r ()
blah = PIO.walkDirRel undefined undefined

walkFSToIO :: Member (Final IO) r => InterpreterFor WalkFS r
walkFSToIO = interpretFinal @IO $ \case
  WalkDir f dir -> do
    let g (a,b,c) = f a b c
    state <- getInitialStateS
    something <- bindS g
    ins <- getInspectorS
    liftS $ PIO.walkDirRel (\a b c -> fromMaybe PIO.WalkFinish . inspect ins <$> something ((a,b,c) <$ state)) dir
-}

data ReadFS m a where
  ReadContents   :: Path b File -> ReadFS m ByteString
  DoesFileExist  :: Path b File -> ReadFS m Bool
  DoesDirExist   :: Path b Dir  -> ReadFS m Bool

readFSToIO :: Member (Embed IO) r => InterpreterFor ReadFS r
readFSToIO = interpret $ \case
  ReadContents file -> embed $ BS.readFile (toFilePath file)
  DoesFileExist file -> PIO.doesFileExist file
  DoesDirExist dir -> PIO.doesDirExist dir
{-# INLINE readFSToIO #-}

readFSVirtual :: Map FilePath VirtualDescriptor -> InterpreterFor ReadFS r
readFSVirtual = undefined

data VirtualDescriptor =
    VirtualFile Text BS.ByteString
  | VirtualDir Text

makeSem_ ''ReadFS

-- | Read a file from the filesystem
readContents  :: Member ReadFS r => Path b File -> Sem r ByteString
doesFileExist :: Member ReadFS r => Path b File -> Sem r Bool
doesDirExist  :: Member ReadFS r => Path b Dir  -> Sem r Bool
