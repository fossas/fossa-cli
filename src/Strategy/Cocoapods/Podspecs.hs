{-# LANGUAGE RecordWildCards #-}
module Strategy.Cocoapods.Podspecs (
  analyze',
  buildGraph,
  parsePodspecFile
) where



import Data.Text (Text, splitOn)
import Data.Void ( Void )
import DepTypes ( VerConstraint (..), Dependency (..), DepType (PodType) )
import Strategy.Ruby.Parse (rubyString, lexeme)
import Text.Megaparsec (Parsec, skipManyTill, anySingle, sepBy, (<|>), eof, try)
import Text.Megaparsec.Char
import Data.String.Conversion (toText, toString)
import Data.List.Extra (head')
import Graphing ( fromList, Graphing )
import Strategy.Cocoapods.Podfile ()
import qualified Data.Map.Strict as Map
import Effect.ReadFS (readContentsParser, ReadFS)
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Path (Path, Abs, File)



analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = do 
  podspecFile <- readContentsParser parsePodspecFile file 
  context "Building dependency graph" $ pure (buildGraph podspecFile)

buildGraph :: PodspecFile -> Graphing Dependency
buildGraph file = Graphing.fromList (map toDependency direct)
  where 
    direct = dependencies file
    toDependency PodspecDependency{..} = 
      Dependency 
        { dependencyType = PodType
        , dependencyName = name
        , dependencyVersion = version 
        , dependencyLocations = mempty 
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        } 

type Parser = Parsec Void Text

data PodspecDependency = PodspecDependency 
  { name :: Text
  , version :: Maybe VerConstraint
  } deriving (Eq, Ord, Show)

newtype PodspecFile = PodspecFile { dependencies :: [PodspecDependency] }

newtype Line = Line PodspecDependency
  deriving (Eq, Ord, Show)

parsePodspecFile :: Parser PodspecFile
parsePodspecFile = linesToPodspecFile (PodspecFile []) . concat <$> ((try parseDepLine <|> parseOtherLine) `sepBy` eol) <* eof

linesToPodspecFile :: PodspecFile -> [Line] -> PodspecFile
linesToPodspecFile file (Line d : xs) = linesToPodspecFile (file{dependencies = d : dependencies file}) xs
linesToPodspecFile file _ = file

parseDepLine :: Parser [Line]
parseDepLine = do
  (n:v:_) <- skipManyTill anySingle (lexeme $ string "dependency") *> sepBy rubyString (char ',') <* newline
  let constraint = readVersionConstraint v
  pure [Line $ PodspecDependency n constraint]

parseOtherLine :: Parser [Line]
parseOtherLine = do
  _ <- skipManyTill anySingle newline 
  pure []











-- analyze' :: (Has ReadFS sig m, Has Diagnostics sig m, Has Exec sig m, Has Logger sig m) => Path Abs File -> m (Graphing Dependency)
-- analyze' file = do
--   contents <- readContentsText file
--   let dependencies = readDependency <$> getDepAssignements $ Data.Text.lines contents
--   context "Building dependency graph" $ pure (buildGraph dependencies)

-- getDepAssignments :: [Text] -> [Dependency]
-- getDepAssignments ls = o where
--   o  = readDependency' <$> as 
--   as = 



-- readDependency' :: (Text, Text) -> Dependency
-- readDependency' (name, versionConstraint) = Dependency {
--     dependencyType = SwiftType,
--     dependencyName = name,
--     dependencyVersion = readVersionConstraint versionConstraint,
--     dependencyLocations = [],
--     dependencyEnvironments = Set.empty,
--     dependencyTags = Map.empty
--   }

-- -- s.dependency 'AlamofireObjectMapper', '~> 5.2'
-- readDependencyLine :: Text -> (Maybe Text, Maybe Text)
-- readDependencyLine s' = let s = strip s' in o where 
--   a = splitOn (toText " ") s'
--   o = if (toText "dependency") `isSuffixOf` (head a)
--         then (Just)

readVersionConstraint :: Text -> Maybe VerConstraint
readVersionConstraint s = vc where
  o = splitOn " " s
  c = head' o
  v = head' $ tail o
  vc = readVersionConstraint' c v


readVersionConstraint' :: Maybe Text -> Maybe Text -> Maybe VerConstraint
readVersionConstraint' Nothing (Just v) = Just $ CEq v
readVersionConstraint' (Just c) (Just v) = Just $ f (parseConstraint c) v where
  f Optimistic version  = CAnd (CGreaterOrEq version) (CLess $ optimisticVersion version)
  f Less version        = CLess version
  f LessOrEq version    = CLessOrEq version
  f Greater version     = CGreater version
  f GreaterOrEq version = CGreaterOrEq version
  f None version        = CEq version 
readVersionConstraint' _ _ = Nothing

data Constraint = Optimistic | Less | LessOrEq | Greater | GreaterOrEq | None

parseConstraint :: Text -> Constraint
parseConstraint c = f $ toString c where
  f "~>" = Optimistic
  f "<"  = Less
  f "<=" = LessOrEq
  f ">"  = Greater
  f ">=" = GreaterOrEq
  f _    = None



optimisticVersion :: Text -> Text
optimisticVersion t = toText v where
  v  = Prelude.init . concat . concat $ zipWith (\b a -> [a, b]) ['.'..] . show <$> Prelude.init vs ++ [succ $ last vs, 0]
  vs = read . toString <$> Prelude.init (splitOn ((toText :: String -> Text) ".") t) :: [Int]