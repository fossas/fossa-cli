module Strategy.Go.Transitive
  ( fillInTransitive
  )
  where

import Prologue

import           Control.Applicative (many)
import qualified Data.Attoparsec.ByteString as A
import           Data.Aeson.Parser
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Vector as V
import           Polysemy
import           Polysemy.Error
import           Polysemy.State

import           Diagnostics
import           Effect.Exec
import           Effect.GraphBuilder
import qualified Graph as G

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
  , modVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Package where
  parseJSON = withObject "Package" $ \obj ->
    Package <$> obj .:  "ImportPath"
            <*> obj .:? "Module"
            <*> obj .:? "Imports"
            <*> obj .:? "Standard"

instance FromJSON Module where
  parseJSON = withObject "Module" $ \obj ->
    Module <$> obj .: "Path"
           <*> obj .: "Version"

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
decodeMany :: FromJSON a => BL.ByteString -> Maybe [a]
decodeMany = decodeWith parser fromJSON
  where
  -- skipSpace is lifted from Data.Aeson.Parser.Internal
  skipSpace = A.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09

  parser = do
    (objects :: [Value]) <- many json <* skipSpace <* A.endOfInput
    pure (Array (V.fromList objects))

-- FIXME: holy bad code, batman. this tightly couples the graph representation to our implementation.
-- this needs to be fixed, probably by replacing `unfold` at the callsites with something that
-- can produce `Map Text G.DepRef`
graphToDepMap :: G.Graph -> Map Text G.DepRef
graphToDepMap = S.foldrWithIndex (\ix dep acc -> M.insert (G.dependencyName dep) (G.DepRef ix) acc) M.empty . G.graphDeps

fillInTransitive :: Members '[Error CLIErr, Exec] r => Path Rel Dir -> G.Graph -> Sem r G.Graph
fillInTransitive dir graph = evalGraphBuilder graph $ runState depMap $ do -- TODO: void????
  goListOutput <- execThrow dir goListCmd []
  case decodeMany goListOutput of
    Nothing -> throw (CommandParseError "" "couldn't parse output of `go list -json all`") -- TODO: command name??
    Just (packages :: [Package]) -> M.traverseWithKey (fillInSingle (indexBy packageImportPath packages)) =<< get

  where
  depMap = graphToDepMap graph

fillInSingle :: forall r. Members '[GraphBuilder, State (Map Text G.DepRef)] r => Map Text Package -> Text -> G.DepRef -> Sem r ()
fillInSingle packages name parentRef = do
  for_ (M.lookup name packages) $
    \(Package _ _ maybeImports system) -> unless (system == Just True) $
      for_ maybeImports $ \imports ->
        for_ imports $ \imported -> do
          maybeChildRef <- addDep imported
          case maybeChildRef of
            Nothing -> pure ()
            Just childRef -> do
              fillInSingle packages imported childRef
              addEdge parentRef childRef

  where
  addDep :: Text -> Sem r (Maybe G.DepRef)
  addDep path = do
    maybeRef <- M.lookup path <$> get @(Map Text G.DepRef)
    case maybeRef of
      Just ref -> pure (Just ref)
      Nothing -> do
        case M.lookup path packages of
          Nothing -> pure Nothing
          Just package ->
            if (packageSystem package == Just True)
              then pure Nothing
              else do
                ref <- addNode (toDependency package)
                modify (M.insert path ref)
                pure (Just ref)

toDependency :: Package -> G.Dependency
toDependency package =
  case packageModule package of
    Nothing -> G.Dependency
      { dependencyType = G.GoType
      , dependencyName = packageImportPath package
      , dependencyVersion = Nothing
      , dependencyLocations = []
      , dependencyTags = M.empty
      }
    Just gomod -> G.Dependency
      { dependencyType = G.GoType
      , dependencyName = modPath gomod
      , dependencyVersion = Just (G.CEq (modVersion gomod))
      , dependencyLocations = []
      , dependencyTags = M.empty
      }

indexBy :: Ord k => (v -> k) -> [v] -> Map k v
indexBy key = M.fromList . map (\v -> (key v, v))
