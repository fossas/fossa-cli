module Strategy.Conda.Naming (
  condaDependencyName,
) where

import Data.Text (Text)

-- | Render a fully-qualified conda dependency name.
--
-- FOSSA resolves conda dependencies by this exact shape, so every strategy
-- that emits a 'Types.CondaType' dependency has to agree on it character for
-- character — a pixi-sourced @zlib@ and a conda-sourced @zlib@ must be the
-- same dependency or the same package resolves twice.
condaDependencyName :: Text -> Text -> Text -> Text
condaDependencyName channel platform name = "'" <> channel <> "':" <> platform <> ":" <> name
