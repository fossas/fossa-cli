module Strategy.Swift.PackageSwift (
  analyzePackageSwift,
  SwiftPackageGitDep (..),
  SwiftPackageGitDepRequirement (..),
  toConstraint,
  isGitRefConstraint,

  -- * for testing,
  buildGraph,
  parsePackageSwiftFile,
  SwiftPackage (..),
  SwiftPackageDep (..),
) where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Effect.Diagnostics (Diagnostics, context, errCtx, fatalText, recover, warnOnErr)
import Control.Monad (void)
import Data.Foldable (asum)
import Data.Map.Strict qualified as Map
import Data.Set (Set, fromList, member)
import Data.Text (Text)
import Data.Void (Void)
import DepTypes (DepType (GitType, SwiftType), Dependency (..), VerConstraint (CEq))
import Diag.Common (MissingDeepDeps (MissingDeepDeps))
import Effect.ReadFS (Has, ReadFS, readContentsJson, readContentsParser)
import Graphing (Graphing, deeps, directs, induceJust, promoteToDirect)
import Path
import Strategy.Swift.Errors (MissingPackageResolvedFile (..))
import Strategy.Swift.PackageResolved (SwiftPackageResolvedFile, resolvedDependenciesOf)
import Text.Megaparsec (
  MonadParsec (takeWhile1P, try),
  Parsec,
  anySingle,
  between,
  empty,
  sepEndBy,
  skipManyTill,
 )
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | Parsing
-- *
type Parser = Parsec Void Text

sc :: Parser ()
sc =
  Lexer.space
    space1
    (Lexer.skipLineComment "//")
    (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: Text -> Parser Text
symbol = Lexer.symbol sc

scWOComment :: Parser ()
scWOComment = Lexer.space space1 empty empty

symbolWOComment :: Text -> Parser Text
symbolWOComment = Lexer.symbol scWOComment

betweenDoubleQuotes :: Parser a -> Parser a
betweenDoubleQuotes = between (symbol "\"") (symbol "\"")

betweenSquareBrackets :: Parser a -> Parser a
betweenSquareBrackets = between (symbol "[") (symbol "]")

betweenBrackets :: Parser a -> Parser a
betweenBrackets = between (symbol "(") (symbol ")")

maybeComma :: Parser ()
maybeComma = void $ optional $ lexeme $ symbol ","

parseQuotedText :: Parser Text
parseQuotedText = betweenDoubleQuotes (lexeme $ takeWhile1P (Just "quoted text") (/= '"'))

parseKeyValue :: Text -> Parser a -> Parser a
parseKeyValue t parser = lexeme $ symbol (t <> ":") *> parser

isEndLine :: Char -> Bool
isEndLine '\n' = True
isEndLine '\r' = True
isEndLine _ = False

-- | Represents https://github.com/apple/swift-package-manager/blob/main/Documentation/PackageDescription.md#methods.
data SwiftPackage = SwiftPackage
  { swiftToolVersion :: Text
  , packageDependencies :: [SwiftPackageDep]
  }
  deriving (Show, Eq, Ord)

-- | Represents https://github.com/apple/swift-package-manager/blob/main/Documentation/PackageDescription.md#package-dependency.
data SwiftPackageDep
  = GitSource SwiftPackageGitDep
  | PathSource Text
  deriving (Show, Eq, Ord)

data SwiftPackageGitDep = SwiftPackageGitDep
  { srcOf :: Text
  , versionRequirement :: Maybe SwiftPackageGitDepRequirement
  }
  deriving (Show, Eq, Ord)

-- | Represents https://github.com/apple/swift-package-manager/blob/main/Documentation/PackageDescription.md#methods-3.
data SwiftPackageGitDepRequirement
  = Branch Text
  | Revision Text
  | Exact Text
  | From Text
  | UpToNextMajor Text
  | UpToNextMinor Text
  | ClosedInterval (Text, Text)
  | RhsHalfOpenInterval (Text, Text)
  deriving (Show, Eq, Ord)

-- Note: Swift fetcher is able to resolve, >=, <, <=, ^, ~ operators.
-- TODO: Leverage `VerConstraint` (CAnd, etc.)
-- TODO: Modify Srclib.Converter.verConstraintToRevision to transform constraint for fetcher
toConstraint :: SwiftPackageGitDepRequirement -> VerConstraint
toConstraint (Branch b) = CEq b
toConstraint (Revision r) = CEq r
toConstraint (Exact e) = CEq e
-- from constraint is equivalent to upToNextMajor
-- Reference: https://github.com/apple/swift-package-manager/blob/main/Documentation/PackageDescription.md#methods-3
toConstraint (From f) = CEq $ "^" <> f
toConstraint (UpToNextMajor c) = CEq $ "^" <> c
toConstraint (UpToNextMinor c) = CEq $ "~" <> c
toConstraint (ClosedInterval (lhs, rhs)) = CEq $ ">=" <> lhs <> " " <> "<=" <> rhs
toConstraint (RhsHalfOpenInterval (lhs, rhs)) = CEq $ ">=" <> lhs <> " " <> "<" <> rhs

isGitRefConstraint :: SwiftPackageGitDepRequirement -> Bool
isGitRefConstraint (Branch _) = True
isGitRefConstraint (Revision _) = True
isGitRefConstraint (Exact _) = True
isGitRefConstraint _ = False

parsePackageDep :: Parser SwiftPackageDep
parsePackageDep = try parsePathDep <|> parseGitDep
  where
    parsePathDep :: Parser SwiftPackageDep
    parsePathDep = PathSource <$> (symbol ".package" *> betweenBrackets (parseKeyValue "path" parseQuotedText))

    parseRequirement :: Text -> Parser Text
    parseRequirement t =
      try (symbol ("." <> t) *> betweenBrackets parseQuotedText)
        <|> parseKeyValue t parseQuotedText

    parseUpToOperator :: Text -> Parser Text
    parseUpToOperator t = symbol ("." <> t) *> betweenBrackets (parseRequirement "from")

    parseRange :: Text -> Parser (Text, Text)
    parseRange rangeOperator = do
      lhs <- parseQuotedText
      _ <- symbol rangeOperator
      rhs <- parseQuotedText
      pure (lhs, rhs)

    optionallyTry :: Parser a -> Parser (Maybe a)
    optionallyTry p = optional . try $ p <* maybeComma

    parseGitDep :: Parser SwiftPackageDep
    parseGitDep = do
      _ <- symbol ".package" <* symbol "("
      _ <- optionallyTry (parseKeyValue "name" parseQuotedText)

      -- Url (Required Field)
      url <- parseKeyValue "url" $ parseQuotedText <* maybeComma

      versionRequirement <-
        optional $
          asum $
            map
              try
              [ Revision <$> parseRequirement "revision"
              , Branch <$> parseRequirement "branch"
              , From <$> parseRequirement "from"
              , Exact <$> parseRequirement "exact"
              , UpToNextMajor <$> parseUpToOperator "upToNextMajor"
              , UpToNextMinor <$> parseUpToOperator "upToNextMinor"
              , ClosedInterval <$> parseRange "..."
              , RhsHalfOpenInterval <$> parseRange "..<"
              ]
      _ <- symbol ")"
      pure $ GitSource $ SwiftPackageGitDep url (versionRequirement)

parsePackageDependencies :: Parser [SwiftPackageDep]
parsePackageDependencies = do
  _ <- lexeme $ skipManyTill anySingle $ symbol "let package = Package("
  skipManyTill anySingle (symbol "dependencies:") *> betweenSquareBrackets (sepEndBy (lexeme parsePackageDep) $ symbol ",")

parseSwiftToolVersion :: Parser Text
parseSwiftToolVersion =
  symbolWOComment "//"
    *> parseKeyValue
      "swift-tools-version"
      (takeWhile1P (Just "swift-tools-version") $ not . isEndLine)

parsePackageSwiftFile :: Parser SwiftPackage
parsePackageSwiftFile = do
  -- Package.swift must specify version for swift tools
  -- https://github.com/apple/swift-package-manager/blob/main/Documentation/PackageDescription.md#about-the-swift-tools-version
  swiftToolVersion <- parseSwiftToolVersion
  SwiftPackage swiftToolVersion <$> parsePackageDependencies

-- | Analysis
-- *
analyzePackageSwift :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> Maybe (Path Abs File) -> m (Graphing.Graphing Dependency)
analyzePackageSwift manifestFile resolvedFile = do
  manifestContent <- context "Identifying dependencies in Package.swift" $ readContentsParser parsePackageSwiftFile manifestFile

  packageResolvedContent <- case resolvedFile of
    Nothing ->
      do
        recover
          . warnOnErr MissingDeepDeps
          . errCtx (MissingPackageResolvedFile manifestFile)
          $ fatalText "Package.resolved file was not discovered"
    Just packageResolved -> context "Identifying dependencies in Package.resolved" $ readContentsJson packageResolved

  context "Building dependency graph" $ pure $ buildGraph manifestContent packageResolvedContent

-- | Graph Building
-- *
buildGraph :: SwiftPackage -> Maybe SwiftPackageResolvedFile -> Graphing.Graphing Dependency
buildGraph manifestContent maybeResolvedContent =
  case maybeResolvedContent of
    Nothing -> induceJust $ directs (map toDependency $ packageDependencies manifestContent)
    -- If dependency (url) is present in the manifest, promote them to direct dependency
    -- Otherwise, keep them as deep dependencies. Since Package.resolved does not include
    -- dependencies sourced from local path, we do not need to do any filtering.
    Just resolvedContent ->
      promoteToDirect (isDirect depInManifest) $
        deeps $
          resolvedDependenciesOf resolvedContent
  where
    isDirect :: Set Text -> Dependency -> Bool
    isDirect s dep = (dependencyName dep) `member` s

    depInManifest :: Set Text
    depInManifest = fromList $ map getName $ packageDependencies manifestContent

    getName :: SwiftPackageDep -> Text
    getName (PathSource path) = path
    getName (GitSource pkg) = srcOf pkg

toDependency :: SwiftPackageDep -> Maybe Dependency
toDependency (PathSource _) = Nothing
toDependency (GitSource pkgDep) =
  Just $
    Dependency
      { dependencyType = depType
      , dependencyName = srcOf pkgDep
      , dependencyVersion = toConstraint <$> versionRequirement pkgDep
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  where
    depType :: DepType
    depType =
      case isGitRefConstraint <$> versionRequirement pkgDep of
        Just True -> GitType
        Just False -> SwiftType
        -- We want to select highest priority tag (descending with semver versioning)
        -- instead of HEAD of the repository
        Nothing -> SwiftType
