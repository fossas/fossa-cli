module Strategy.Go.Transitive
  ( fillInTransitive
  )
  where

import Prologue hiding (empty)

import           Control.Applicative (many)
import qualified Data.Attoparsec.ByteString as A
import           Data.Aeson.Internal (formatError, iparse)
import           Data.Aeson.Parser
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector as V
import           Polysemy
import           Polysemy.Error

import           Diagnostics
import           Effect.Exec
import           Effect.Graphing
import           Strategy.Go.Types

goListCmd :: Command
goListCmd = Command
  { cmdNames = ["go"]
  , cmdBaseArgs = ["list", "-json", "all"]
  , cmdAllowErr = NonEmptyStdout
  }

data Package = Package
  { packageImportPath :: Text
  , packageModule     :: Maybe Module
  , packageImports    :: Maybe [Text]
  , packageSystem     :: Maybe Bool
  } deriving (Eq, Ord, Show, Generic)

data Module = Module
  { modPath    :: Text
  , modVersion :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Package where
  parseJSON = withObject "Package" $ \obj ->
    Package <$> obj .:  "ImportPath"
            <*> obj .:? "Module"
            <*> obj .:? "Imports"
            <*> obj .:? "Standard"

instance FromJSON Module where
  parseJSON = withObject "Module" $ \obj ->
    Module <$> obj .:  "Path"
           <*> obj .:? "Version"

-- `go list -json` is dumb: it outputs a bunch of raw json objects:
--     {
--       ...
--     }
--     {
--       ...
--     }
-- decodeMany is our workaround. it produces `[a]` by repeatedly parsing
-- json objects, wrapping them into `[Value]`, then decoding `[Value]`
-- into `[a]`
decodeMany :: FromJSON a => BL.ByteString -> Either (JSONPath, String) [a]
decodeMany = eitherDecodeWith parser (iparse parseJSON)
  where
  -- skipSpace is lifted from Data.Aeson.Parser.Internal
  skipSpace = A.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09

  parser = do
    (objects :: [Value]) <- many json <* skipSpace <* A.endOfInput
    pure (Array (V.fromList objects))


graphTransitive :: Member (Graphing GolangPackage) r => [Package] -> Sem r ()
graphTransitive = void . traverse_ go
  where
  go :: Member (Graphing GolangPackage) r => Package -> Sem r ()
  go package = unless (packageSystem package == Just True) $ do
    let -- when a gomod field is present, use that for the package import path
        -- otherwise use the top-level package import path
        path :: Text
        path = maybe (packageImportPath package) modPath (packageModule package)

        pkg :: GolangPackage
        pkg = mkGolangPackage path

    traverse_ (traverse_ (edge pkg . mkGolangPackage)) (packageImports package)

    -- when we have a gomod, and that gomod has a version, add label for version
    case modVersion =<< packageModule package of
      Nothing -> pure ()
      Just ver -> label pkg (mkGolangVersion ver)


fillInTransitive :: Members '[Error ExecErr, Exec, Graphing GolangPackage] r => Path Rel Dir -> Sem r ()
fillInTransitive dir = do
  goListOutput <- execThrow dir goListCmd []
  case decodeMany goListOutput of
    Left (path, err) -> throw (CommandParseError "go list -json all" (T.pack (formatError path err)))
    Right (packages :: [Package]) -> graphTransitive packages
