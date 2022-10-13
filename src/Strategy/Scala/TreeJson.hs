{-# LANGUAGE RecordWildCards #-}

module Strategy.Scala.TreeJson (
  analyze,
  SbtTreeJson (..),
  SbtTree (..),
  parseSbtArtifact,
) where

import Control.Effect.Diagnostics (Diagnostics)
import Control.Monad (void)
import Control.Monad.Identity (Identity)
import Data.Aeson (
  FromJSON (parseJSON),
  withArray,
  withObject,
  (.:),
 )
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Vector (toList)
import Data.Void (Void)
import DepTypes (
  DepType (MavenType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Grapher (GrapherC, Has, deep, direct, edge, evalGrapher, run)
import Effect.ReadFS (ReadFS, readContentsJson)
import Graphing (Graphing, shrinkRoots)
import Path (Abs, File, Path)
import Strategy.Scala.Common (SbtArtifact (..))
import Text.Megaparsec (
  MonadParsec (try),
  Parsec,
  empty,
  errorBundlePretty,
  optional,
  runParser,
  some,
  (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | Represents output of Sbt's Treeview command.
-- Ref: https://github.com/sbt/sbt/blob/master/main/src/main/scala/sbt/internal/graph/rendering/TreeView.scala#L22
--
-- It looks like:
-- > [
-- >   {
-- >     "text": "groupId:artifactId:version",
-- >     "children": [
-- >       {
-- >         "text": "...",
-- >         "children": [
-- >           ...
-- >         ]
-- >       },
-- >     ]
-- >   }
-- > ]
-- -
newtype SbtTreeJson = SbtTreeJson [SbtTree]
  deriving (Eq, Ord, Show)

data SbtTree = SbtTree
  { artifact :: SbtArtifact
  , dependsOn :: [SbtTree]
  }
  deriving (Show, Eq, Ord)

instance FromJSON SbtTreeJson where
  parseJSON = withArray "SbtTreeRoot" $ \arr -> do
    SbtTreeJson . toList <$> traverse parseJSON arr

instance FromJSON SbtTree where
  parseJSON = withObject "SbtTree" $ \obj -> do
    txt <- obj .: "text"
    children <- obj .: "children"

    artifact <- case runParser parseSbtArtifact "sbtArtifact" txt of
      Left err -> fail $ errorBundlePretty err
      Right sa -> pure sa
    pure $ SbtTree artifact children

type ParserT = Parsec Void Text

parseSbtArtifact :: ParserT SbtArtifact
parseSbtArtifact = do
  groupId <- parseValidProjectIdentifier
  artifactId <- (":" >> parseValidProjectIdentifier)
  version <- lexeme (":" >> parseValidProjectIdentifier)
  evictedByVersion <- try . optional $ parseEviction
  pure $
    SbtArtifact
      groupId
      artifactId
      (fromMaybe version evictedByVersion)
  where
    sc :: ParserT ()
    sc = Lexer.space (void $ some $ char ' ' <|> char '\t') empty empty

    lexeme :: ParserT a -> ParserT a
    lexeme = Lexer.lexeme sc

    symbol :: Text -> ParserT Text
    symbol = Lexer.symbol sc

    parseEviction :: ParserT (Text)
    parseEviction = lexeme ("(evicted by" <|> "(evicted by") *> parseValidProjectIdentifier <* symbol ")"

    parseValidProjectIdentifier :: ParserT Text
    parseValidProjectIdentifier = toText <$> some (alphaNumChar <|> char '.' <|> char '-' <|> char '_')

buildGraph :: SbtTreeJson -> Graphing Dependency
buildGraph (SbtTreeJson tress) = shrinkRoots . run . evalGrapher $ buildGraph'
  where
    buildGraph' :: GrapherC Dependency Identity ()
    buildGraph' = do
      for_ tress $ \root -> do
        direct $ toDependency (artifact root)
        unfold root

    unfold candidate = do
      let parent = toDependency (artifact candidate)
      deep parent
      for_ (dependsOn candidate) $ \child -> do
        let childDep = toDependency (artifact child)
        edge parent childDep
        unfold child

    toDependency :: SbtArtifact -> Dependency
    toDependency SbtArtifact{..} =
      Dependency
        { dependencyType = MavenType
        , dependencyName = toText groupId <> ":" <> toText artifactId
        , dependencyVersion = Just (CEq version)
        , dependencyLocations = mempty
        , dependencyEnvironments = mempty
        , dependencyTags = mempty
        }

analyze ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  Path Abs File ->
  m (Graphing Dependency)
analyze treeJsonPath = buildGraph <$> readContentsJson treeJsonPath
