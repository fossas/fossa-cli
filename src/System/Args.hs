module System.Args (
  redactApiKeyFromCmdArgs,
  getCommandArgs,
) where

import App.Fossa.Config.Common (fossaApiKeyCmdText)
import Data.List (elemIndex)
import Data.String.Conversion (ToText (toText))
import Data.Text (Text, isPrefixOf)
import GHC.Environment qualified as Environment

getCommandArgs :: IO [Text]
getCommandArgs = redactApiKeyFromCmdArgs . map toText <$> Environment.getFullArgs

-- | Redacts Api Key from raw command args.
redactApiKeyFromCmdArgs :: [Text] -> [Text]
redactApiKeyFromCmdArgs = redactApiKeyWhenDirectlySupplied . redactApiKeyWhenPassedAsArg
  where
    -- When provided with --fossa-api-key=someKey
    redactApiKeyWhenDirectlySupplied :: [Text] -> [Text]
    redactApiKeyWhenDirectlySupplied =
      map
        ( \arg ->
            if ("--" <> fossaApiKeyCmdText <> "=") `isPrefixOf` arg
              then ("--" <> fossaApiKeyCmdText <> "=<REDACTED>")
              else arg
        )

    -- When provided with --fossa-api-key someKey
    redactApiKeyWhenPassedAsArg :: [Text] -> [Text]
    redactApiKeyWhenPassedAsArg args =
      case elemIndex ("--" <> fossaApiKeyCmdText) args of
        Nothing -> args
        Just i -> replaceAtWith args (i + 1) "<REDACTED>"

    replaceAtWith :: [Text] -> Int -> Text -> [Text]
    replaceAtWith args replaceAt replaceWith = case splitAt replaceAt args of
      (start, _ : end) -> start ++ replaceWith : end
      _ -> args
