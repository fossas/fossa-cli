{-# LANGUAGE RecordWildCards #-}

module Graphing.Trace (
  GraphTrace (toTrace),
  traceGraph,
  renderGvFile,
) where

import Algebra.Graph.Acyclic.AdjacencyMap qualified as Acyclic
import Algebra.Graph.AdjacencyMap qualified as Cyclic
import Control.Effect.Lift (Has, Lift, sendIO)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Extra (showT)
import Data.Text.IO qualified as TIO
import DepTypes (DepEnvironment, Dependency (..), VerConstraint (..))
import Graphing (Graphing (unGraphing), Node (..))

gvFileName :: (IsString a) => a
gvFileName = "./fossa.debug.gv"

class Ord a => GraphTrace a where
  toTrace :: a -> Text

instance GraphTrace a => GraphTrace (Node a) where
  toTrace Root = "ROOT"
  toTrace (Node a) = toTrace a

instance GraphTrace Dependency where
  toTrace Dependency{..} = Text.unwords [dependencyName, version, envs]
    where
      version = maybe "*" versionTxt dependencyVersion
      envs = "[" <> envsToText dependencyEnvironments <> "]"

envsToText :: Set DepEnvironment -> Text
envsToText = Text.intercalate ", " . map showT . Set.toList

versionTxt :: VerConstraint -> Text
versionTxt = \case
  CEq txt -> txt
  CURI txt -> txt
  CCompatible txt -> txt
  CAnd vc vc' -> versionTxt vc <> "AND" <> versionTxt vc'
  COr vc vc' -> versionTxt vc <> "OR" <> versionTxt vc'
  CLess txt -> txt
  CLessOrEq txt -> txt
  CGreater txt -> txt
  CGreaterOrEq txt -> txt
  CNot txt -> txt

data EmittedGraph
  = Cyclic [Text]
  | Acyclic [Text]

traceGraph :: Has (Lift IO) sig m => GraphTrace a => Graphing a -> m ()
traceGraph gr = sendIO $ do
  let graphTxt = renderGvFile gr
  TIO.writeFile gvFileName graphTxt
{-# WARNING traceGraph "This function is for debug purposes only and should not be used in production." #-}

renderGvFile :: GraphTrace a => Graphing a -> Text
renderGvFile gr = emit graphEdges
  where
    adjMap = unGraphing gr
    graphEdges = case Acyclic.toAcyclic adjMap of
      Nothing -> emitGraphEdges adjMap
      Just ac -> emitDiGraphEdges ac

emit :: EmittedGraph -> Text
emit (Cyclic edges) = Text.unlines (["graph G {"] <> map (indent 4) edges <> ["}"])
emit (Acyclic edges) = Text.unlines (["digraph G {"] <> map (indent 4) edges <> ["}"])

emitDiGraphEdges :: GraphTrace a => Acyclic.AdjacencyMap a -> EmittedGraph
emitDiGraphEdges = Acyclic . edgesToTexts (Icon "->") . Acyclic.edgeList

emitGraphEdges :: GraphTrace a => Cyclic.AdjacencyMap a -> EmittedGraph
emitGraphEdges = Cyclic . edgesToTexts (Icon "--") . Cyclic.edgeList

edgesToTexts :: GraphTrace a => Icon -> [(a, a)] -> [Text]
edgesToTexts icon = map (renderEdge icon . traceTuple)

traceTuple :: GraphTrace a => (a, a) -> (Text, Text)
traceTuple = bimap toTrace toTrace

---------------------- HELPERS ------------------------

newtype Icon = Icon Text

renderEdge :: Icon -> (Text, Text) -> Text
renderEdge (Icon icon) (parent, child) = terminated $ Text.unwords [quoted parent, icon, quoted child]

quoted :: Text -> Text
quoted item = "\"" <> item <> "\""

terminated :: Text -> Text
terminated = (<> ";")

indent :: Int -> Text -> Text
indent count txt = Text.replicate count " " <> txt
