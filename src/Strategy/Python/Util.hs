module Strategy.Python.Util (
  buildGraph,
  buildGraphSetupFile,
  Version (..),
  Marker (..),
  MarkerOp (..),
  Operator (..),
  Req (..),
  requirementParser,
  reqToDependency,
  toConstraint,
) where

import Control.Monad (join)
import Control.Monad.Identity (Identity)
import Data.Char qualified as C
import Data.Foldable (asum, find, for_)
import Data.Map.Strict qualified as Map
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import DepTypes
import Effect.Grapher (Grapher, GrapherC, Has, deep, direct, edge, evalGrapher, run)
import Graphing (Graphing)
import Graphing qualified
import Strategy.Python.Pip (PythonPackage (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.URI qualified as URI
import Toml qualified
import Toml.Schema qualified

pkgToReq :: PythonPackage -> Req
pkgToReq p =
  NameReq (pkgName p) Nothing (Just [Version OpEq (pkgVersion p)]) Nothing

depName :: Req -> Text
depName (NameReq nm _ _ _) = nm
depName (UrlReq nm _ _ _) = nm

reqToDependency :: Req -> Dependency
reqToDependency req =
  Dependency
    { dependencyType = PipType
    , dependencyName = depName req
    , dependencyVersion = depVersion req
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = maybe Map.empty toTags (depMarker req)
    }
  where
    depVersion (NameReq _ _ versions _) = toConstraint <$> versions
    depVersion (UrlReq _ _ uri _) = Just (CURI (URI.render uri))

    depMarker (NameReq _ _ _ marker) = marker
    depMarker (UrlReq _ _ _ marker) = marker

buildGraphSetupFile :: Maybe [PythonPackage] -> Maybe Text -> [Req] -> Maybe Text -> [Req] -> Graphing Dependency
buildGraphSetupFile maybePackages pyPackageName pyReqs cfgPackageName cfgReqs = do
  Graphing.gmap reqToDependency $ do
    case maybePackages of
      Nothing -> Graphing.fromList (pyReqs ++ cfgReqs)
      Just packages -> do
        run . evalGrapher $ do
          addDeps packages pyPackageName pyReqs
          addDeps packages cfgPackageName cfgReqs
  where
    addDeps :: [PythonPackage] -> Maybe Text -> [Req] -> GrapherC Req Identity ()
    addDeps packages maybeName reqs = do
      case maybeName of
        Nothing -> for_ reqs direct
        Just packageName ->
          case (find (\p -> Text.toLower (pkgName p) == Text.toLower (packageName)) packages) of
            Nothing -> for_ reqs direct
            Just pkg ->
              for_ (requires pkg) $ \c -> do
                let r = pkgToReq c
                direct r
                addChildren r c

buildGraph :: Maybe [PythonPackage] -> [Req] -> Graphing Dependency
buildGraph maybePackages reqs = do
  Graphing.gmap reqToDependency $ do
    case maybePackages of
      Nothing -> Graphing.fromList reqs
      Just packages -> do
        run . evalGrapher $ do
          for_ reqs direct
          for_ packages $ \p -> do
            case findParent (pkgName p) of
              Just parent -> addChildren parent p
              Nothing -> pure ()
  where
    findParent :: Text -> Maybe Req
    findParent packageName = find (\r -> Text.toLower (depName r) == Text.toLower (packageName)) reqs

addChildren :: (Has (Grapher Req) sig m) => Req -> PythonPackage -> m ()
addChildren parent pkg = do
  for_ (requires pkg) $ \c -> do
    let child = pkgToReq c
    deep child
    edge parent child
    addChildren child c

-- we pull out tags naively. we don't respect and/or semantics, and ignore operators
-- FUTURE: more useful tagging? in particular: only pull out sys_platform?
toTags :: Marker -> Map.Map Text [Text]
toTags = Map.fromListWith (++) . map (\(a, b) -> (a, [b])) . go
  where
    go (MarkerAnd a b) = go a ++ go b
    go (MarkerOr a b) = go a ++ go b
    go (MarkerExpr lhs op rhs) =
      case op of
        MarkerIn -> [(lhs, rhs)]
        MarkerNotIn -> [(lhs, "not (" <> rhs <> ")")]
        MarkerOperator _ -> [(lhs, rhs)]

toConstraint :: [Version] -> VerConstraint
toConstraint = foldr1 CAnd . map (\(Version op ver) -> opToConstraint op ver)
  where
    opToConstraint = \case
      OpCompatible -> CCompatible
      OpEq -> CEq
      OpNot -> CNot
      OpLtEq -> CLessOrEq
      OpGtEq -> CGreaterOrEq
      OpLt -> CLess
      OpGt -> CGreater
      OpArbitrary -> CEq

type Parser = Parsec Void Text

data Version = Version
  { versionOperator :: Operator
  , versionVersion :: Text
  }
  deriving (Eq, Ord, Show)

data Marker
  = MarkerAnd Marker Marker
  | MarkerOr Marker Marker
  | MarkerExpr Text MarkerOp Text -- marker_var marker_op marker_var
  deriving (Eq, Ord, Show)

data MarkerOp
  = MarkerIn
  | MarkerNotIn
  | MarkerOperator Operator
  deriving (Eq, Ord, Show)

data Operator
  = -- | @~=@; equivalent to `>= V.N, == V.*`
    OpCompatible
  | -- | @==@
    OpEq
  | -- | @!=@
    OpNot
  | -- | @<=@
    OpLtEq
  | -- | @>=@
    OpGtEq
  | -- | @<@
    OpLt
  | -- | @>@
    OpGt
  | -- | @===@
    OpArbitrary
  deriving (Eq, Ord, Show)

data Req
  = NameReq Text (Maybe [Text]) (Maybe [Version]) (Maybe Marker) -- name, extras, ...
  | UrlReq Text (Maybe [Text]) URI.URI (Maybe Marker) -- name, extras, ...
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue Req where
  fromValue v = do
    value <- Toml.Schema.fromValue v
    case parseReq value of
      Left _ -> Toml.Schema.failAt (Toml.valueAnn v) "invalid req"
      Right r -> pure r

parseReq :: Text -> Either Text Req
parseReq candidate = case runParser requirementParser "" candidate of
  Left peb -> Left $ toText $ errorBundlePretty peb
  Right rr -> Right rr

-- grammar extracted from https://www.python.org/dev/peps/pep-0508/
requirementParser :: Parser Req
requirementParser = specification
  where
    oneOfS = asum . map string
    isSpace c = c == ' ' || c == '\t'

    whitespace = takeWhileP (Just "whitespace") isSpace :: Parser Text
    whitespace1 = label "whitespace1" $ takeWhile1P (Just "whitespace1") isSpace :: Parser Text
    letterOrDigit = label "letterOrDigit" $ satisfy (\c -> C.isLetter c || C.isDigit c)

    version_cmp = label "version_cmp" $ whitespace *> version_operator

    version_operator =
      label "version_operator" $
        OpCompatible <$ string "~="
          <|> OpLtEq <$ string "<="
          <|> OpGtEq <$ string ">="
          <|> OpNot <$ string "!="
          <|> OpArbitrary <$ string "==="
          <|> OpEq <$ string "=="
          <|> OpLt <$ string "<"
          <|> OpGt <$ string ">"

    version = label "version" $ whitespace *> some (letterOrDigit <|> oneOf ['-', '_', '.', '*', '+', '!'])
    version_one = label "version_one" $ Version <$> version_cmp <*> (toText <$> version) <* whitespace
    version_many = label "version_many" $ version_one `sepBy1` (whitespace *> char ',')
    versionspec = label "versionspec" $ between (char '(') (char ')') version_many <|> version_many
    urlspec = label "urlspec" $ char '@' *> whitespace *> URI.parser

    marker_op =
      label "marker_op" $
        MarkerOperator <$> version_cmp
          <|> MarkerIn <$ whitespace <* string "in"
          <|> MarkerNotIn <$ whitespace <* string "not" <* whitespace1 <* string "in"
    python_str_c :: Parser Char
    python_str_c =
      label "python_str_c" $
        satisfy isSpace
          <|> satisfy C.isLetter
          <|> satisfy C.isDigit
          <|> oneOf ("().{}-_*#:;,/?[]!~`@$%^&=+|<>" :: String)

    dquote :: Parser Char
    dquote = label "dquote" $ char '\"'
    squote :: Parser Char
    squote = label "squote" $ char '\''

    python_str =
      label "python_str" $
        (squote *> many (python_str_c <|> dquote) <* squote)
          <|> (dquote *> many (python_str_c <|> squote) <* dquote)

    env_var :: Parser Text
    env_var =
      label "env_var" $
        oneOfS
          [ "python_version"
          , "python_full_version"
          , "os_name"
          , "sys_platform"
          , "platform_release"
          , "platform_system"
          , "platform_version"
          , "platform_machine"
          , "platform_python_implementation"
          , "implementation_name"
          , "implementation_version"
          , "extra"
          ]
    marker_var :: Parser Text
    marker_var = label "marker_var" $ whitespace *> (env_var <|> fmap toText python_str)
    marker_expr :: Parser Marker
    marker_expr = label "marker_expr" $
      MarkerExpr <$> marker_var <*> marker_op <*> marker_var
        <|> whitespace *> char '(' *> marker_or <* char ')'

    marker_and :: Parser Marker
    marker_and = label "marker_and" $ do
      first <- marker_expr
      rest <- many (try $ whitespace *> string "and" *> whitespace *> marker_expr)
      pure $ foldl MarkerAnd first rest

    marker_or :: Parser Marker
    marker_or = label "marker_or" $ do
      first <- marker_and
      rest <- many (try $ whitespace *> string "or" *> whitespace *> marker_and)
      pure $ foldl MarkerOr first rest

    marker = label "marker" marker_or
    quoted_marker = label "quoted_marker" $ char ';' *> whitespace *> marker

    identifier_end =
      label "identifier_end" $
        pure <$> letterOrDigit
          <|> do
            special <- many (oneOf ['-', '_', '.'])
            lod <- letterOrDigit
            pure (special ++ [lod])
    identifier = label "identifier" $ (:) <$> letterOrDigit <*> (concat <$> many identifier_end)
    name = label "name" $ toText <$> identifier
    extras_list :: Parser [Text]
    extras_list =
      label "extras_list" $
        (toText <$> identifier)
          `sepBy` (whitespace *> char ',' <* whitespace)
    extras = label "extras" $ char '[' *> whitespace *> optional extras_list <* whitespace <* char ']'

    name_req = label "name_req" $ NameReq <$> name <* whitespace <*> (join <$> optional extras) <* whitespace <*> optional versionspec <* whitespace <*> optional quoted_marker
    url_req = label "url_req" $ UrlReq <$> name <* whitespace <*> (join <$> optional extras) <* whitespace <*> urlspec <* whitespace <*> optional quoted_marker

    specification = label "specification" $ whitespace *> (try url_req <|> name_req) <* whitespace
