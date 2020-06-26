module Strategy.NuGet.Paket
  ( discover
  , analyze
  , findSections
  , buildGraph

  , PaketDep(..)
  , Section(..)
  , Remote(..)
  ) where

import Prologue

import Control.Effect.Diagnostics
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Char as C

import DepTypes
import Discovery.Walk
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Types
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "paket.lock") files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "paket-paketlock" DotnetGroup $ analyze file

  pure WalkContinue

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsParser findSections file

mkProjectClosure :: Path Abs File -> [Section] -> ProjectClosureBody
mkProjectClosure file sections = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph sections
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = Complete
    }

newtype PaketPkg = PaketPkg { pkgName :: Text }
  deriving (Eq, Ord, Show, Generic)

type PaketGrapher = LabeledGrapher PaketPkg PaketLabel

data PaketLabel =
    PaketVersion Text
  | PaketRemote Text
  | GroupName Text
  | DepLocation Text
  deriving (Eq, Ord, Show, Generic)

toDependency :: PaketPkg -> Set PaketLabel -> Dependency
toDependency pkg = foldr applyLabel start
  where

  start :: Dependency
  start = Dependency
    { dependencyType = NuGetType
    , dependencyName = pkgName pkg
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }

  applyLabel :: PaketLabel -> Dependency -> Dependency
  applyLabel (PaketVersion ver) dep = dep { dependencyVersion = Just (CEq ver) }
  applyLabel (GroupName name) dep = dep { dependencyTags = M.insertWith (++) "group" [name] (dependencyTags dep) }
  applyLabel (DepLocation loc) dep = dep { dependencyTags = M.insertWith (++) "location" [loc] (dependencyTags dep) }
  applyLabel (PaketRemote repo) dep = dep { dependencyLocations = repo : dependencyLocations dep }
  
buildGraph :: [Section] -> Graphing Dependency
buildGraph sections = run . withLabeling toDependency $
      traverse_ (addSection "MAIN") sections
      where
            addSection :: Has PaketGrapher sig m => Text -> Section -> m ()
            addSection group (StandardSection location remotes) = traverse_ (addRemote group location) remotes
            addSection _ (GroupSection group gSections) = traverse_ (addSection group) gSections
            addSection _ (UnknownSection _) = pure ()

            addRemote :: Has PaketGrapher sig m => Text -> Text -> Remote -> m ()
            addRemote group loc remote = traverse_ (addSpec (DepLocation loc) (PaketRemote $ location remote) (GroupName group)) (dependencies remote) 

            addSpec :: Has PaketGrapher sig m => PaketLabel -> PaketLabel -> PaketLabel -> PaketDep -> m ()
            addSpec loc remote group dep = do
              -- add edges, labels, and direct deps
              let pkg = PaketPkg (name dep)
              traverse_ (edge pkg . PaketPkg) (transitive dep)
              label pkg (PaketVersion (version dep))
              label pkg remote
              label pkg group
              label pkg loc
              direct pkg

type Name = Text
type Location = Text

data Section =
       StandardSection Location [Remote]
      | UnknownSection Text
      | GroupSection Name [Section]
      deriving (Eq, Ord, Show, Generic)

data Remote = Remote
     { location      :: Text
     , dependencies  :: [PaketDep]
     } deriving (Eq, Ord, Show, Generic)

data PaketDep = PaketDep
     { name       :: Text
     , version    :: Text
     , transitive :: [Text]
     } deriving (Eq, Ord, Show, Generic)

data Group = Group
     { groupName    :: Text
     , groupSections :: [Section]
     } deriving (Eq, Ord, Show, Generic)


findSections :: Parser [Section]
findSections = many (try standardSectionParser <|> try groupSection <|> try unknownSection) <* eof

groupSection :: Parser Section
groupSection = do
          _ <- chunk "GROUP"
          name <- textValue
          sections <- many (try standardSectionParser <|>  try unknownSection)
          pure $ GroupSection name sections

unknownSection :: Parser Section
unknownSection = do 
      emptyLine <- restOfLine
      guard $ not $ "GROUP" `T.isPrefixOf` emptyLine
      _ <- eol
      pure $ UnknownSection emptyLine

standardSectionParser :: Parser Section
standardSectionParser = L.nonIndented scn $ L.indentBlock scn $ do
          location <- chunk "HTTP" <|> "GITHUB" <|> "NUGET"
          return $ L.IndentMany Nothing (\remotes -> pure $ StandardSection location remotes) remoteParser

remoteParser :: Parser Remote
remoteParser = L.indentBlock scn $ do
      _ <- chunk "remote:"
      value <- textValue
      pure $ L.IndentMany Nothing (\specs -> pure $ Remote value specs) specParser

specParser :: Parser PaketDep
specParser = L.indentBlock scn $ do
        name <- findDep
        version <- findVersion
        _ <- restOfLine
        pure $ L.IndentMany Nothing (\a -> pure $ PaketDep name version a) specsParser
      
specsParser :: Parser Text
specsParser = findDep <* restOfLine

findDep :: Parser Text
findDep = lexeme (takeWhile1P (Just "dep") (not . C.isSpace))

findVersion :: Parser Text
findVersion = between (char '(') (char ')') $ takeWhile1P (Just "version") (/= ')')

textValue :: Parser Text
textValue = chunk " " *> restOfLine

restOfLine :: Parser Text
restOfLine = takeWhileP (Just "ignored") (not . isEndLine)

isEndLine :: Char -> Bool
isEndLine '\n' = True
isEndLine '\r' = True
isEndLine _    = False

scn :: Parser ()
scn =  L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc =  L.space (void $ some (char ' ')) empty empty
