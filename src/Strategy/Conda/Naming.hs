module Strategy.Conda.Naming (
  condaDependencyName,
) where

import Data.Text (Text)

-- | Conda dependency name format required by FOSSA.
condaDependencyName :: Text -> Text -> Text -> Text
condaDependencyName channel platform name = "'" <> channel <> "':" <> platform <> ":" <> name
