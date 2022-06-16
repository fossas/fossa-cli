module Tryout (main) where

import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Church
import Control.Concurrent (threadDelay)
import Control.Effect.Lift
import Data.ByteString (ByteString, readFile)
import Data.Foldable (traverse_)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Text.IO (putStrLn)
import Prelude hiding (putStrLn, readFile)

newtype AppConfig = AppConfig FilePath
newtype ValidValue = ValidValue Text deriving (Eq, Ord)
type SeenValues = Set ValidValue

-- The super-magic file reading utility!!
-- For implemetation, we'll just return Nothing when the file is not
-- valid UTF-8.
magicExtractor :: ByteString -> Maybe Text
magicExtractor = either (const Nothing) Just . decodeUtf8'

-- Helper for our ValidValue newtype
extractValue :: ByteString -> Maybe ValidValue
extractValue bs = ValidValue <$> magicExtractor bs

-- We don't want to retry reading immediately.
sleepForOneSecond :: IO ()
sleepForOneSecond = threadDelay 1000000

-- Stand-in for sophisticated config logic
readConfig :: IO AppConfig
readConfig = pure $ AppConfig "path/to/file"

-- Helper for reading values from the config
takeConfig :: AppConfig -> FilePath
takeConfig (AppConfig fp) = fp

-- Helper for getting printable values
takeValue :: ValidValue -> Text
takeValue (ValidValue txt) = txt

-- ==============================
-- = NO TRANSFORMERS OR EFFECTS =
-- ==============================

-- main :: IO ()
-- main = do
--   conf <- readConfig
--   values <- doTheProgram conf mempty
--   traverse_ (putStrLn . takeValue) values

-- doTheProgram :: AppConfig -> SeenValues -> IO [ValidValue]
-- doTheProgram conf seenValues = do
--   -- Get the value from the file
--   -- Throws an error if the file is missing
--   contents <- readFile $ takeConfig conf

--   -- Use the magic reader function, with our wrapper
--   -- We're matching on a (Maybe ValidValue)
--   case extractValue contents of
--     -- Terminate the recursion
--     Nothing -> pure []
--     -- We have a valid value
--     Just value -> do
--       if value `Set.member` seenValues
--         then do
--           -- We've already seen this value, just reread
--           sleepForOneSecond
--           doTheProgram conf seenValues
--         else do
--           -- We have a new value, we'll add it to our list builder
--           sleepForOneSecond
--           -- Don't let this value repeat itself
--           let seenValues' = Set.insert value seenValues
--           -- Recurse into the next read operation
--           values <- doTheProgram conf seenValues'
--           -- Once recursion has terminated, return the list builder
--           pure (value : values)

-- main :: IO ()
-- main = do
--   conf <- readConfig
--   values <- execWriterT $ (`evalStateT` mempty) $ (`runReaderT` conf) doTheProgram
--   traverse_ (putStrLn . takeValue) values

-- doTheProgram ::
--   ( MonadReader AppConfig m
--   , MonadWriter [ValidValue] m
--   , MonadState SeenValues m
--   , MonadIO m
--   ) =>
--   m ()
-- doTheProgram = do
--   -- Get the config from the reader
--   filepath <- asks takeConfig
--   -- Read the actual file
--   contents <- liftIO $ readFile filepath
--   case extractValue contents of
--     -- Value is invalid, terminate
--     Nothing -> pure ()
--     Just value -> do
--       isNewValue <- gets (Set.member value)
--       modify' (Set.insert value)
--       if isNewValue
--         then do
--           tell [value]
--           liftIO sleepForOneSecond
--           doTheProgram
--         else doTheProgram
-- ReaderT AppConfig (StateT (Set ValidValue) (WriterT [ValidValue] IO)) ()

main :: IO ()
main = do
  conf <- readConfig
  values <- execWriter @[ValidValue] . evalState @SeenValues mempty $ runReader conf doTheProgram
  traverse_ (putStrLn . takeValue) values

doTheProgram ::
  ( Has (Reader AppConfig) sig m
  , Has (Writer [ValidValue]) sig m
  , Has (State SeenValues) sig m
  , Has (Lift IO) sig m
  ) =>
  m ()
doTheProgram = do
  -- Get the config from the reader
  filepath <- asks takeConfig
  -- Read the actual file
  contents <- sendIO $ readFile filepath
  case extractValue contents of
    -- Value is invalid, terminate
    Nothing -> pure ()
    Just value -> do
      -- Check if the value existed previously
      isNewValue <- gets (Set.member value)
      -- We've seen this value before
      modify (Set.insert value)
      if isNewValue
        then do
          -- This is a new value
          tell [value]
          sendIO sleepForOneSecond
          doTheProgram
        else doTheProgram
