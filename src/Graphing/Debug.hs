{-# LANGUAGE RecordWildCards #-}

module Graphing.Debug (
  GraphDebug (toDebug),
  debugGraph,
  renderGvFile,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
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

class Ord a => GraphDebug a where
  toDebug :: a -> Text

instance GraphDebug a => GraphDebug (Node a) where
  toDebug Root = "ROOT"
  toDebug (Node a) = toDebug a

instance GraphDebug Dependency where
  toDebug Dependency{..} = Text.unwords [dependencyName, version, envs]
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

newtype EdgeText = EdgeText
  { unEdgeText :: Text
  }

debugGraph :: Has (Lift IO) sig m => GraphDebug a => Graphing a -> m ()
debugGraph gr = sendIO $ do
  let graphTxt = renderGvFile gr
  TIO.writeFile gvFileName graphTxt
{-# WARNING debugGraph "This function is for debug purposes only and should not be used in production." #-}

renderGvFile :: GraphDebug a => Graphing a -> Text
renderGvFile = emit . emitDiGraphEdges . unGraphing

emit :: [EdgeText] -> Text
emit edges = Text.unlines (["digraph G {"] <> map (indent 4 . unEdgeText) edges <> ["}"])

emitDiGraphEdges :: GraphDebug a => AM.AdjacencyMap a -> [EdgeText]
emitDiGraphEdges = edgesToTexts (Icon "->") . AM.edgeList

edgesToTexts :: GraphDebug a => Icon -> [(a, a)] -> [EdgeText]
edgesToTexts icon = map (EdgeText . renderEdge icon . debugTuple)

debugTuple :: GraphDebug a => (a, a) -> (Text, Text)
debugTuple = bimap toDebug toDebug

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
