-- | Parses RegistryImageSource.
--
-- This module parses "NAME[:TAG|@DIGEST]" format passed
-- when using docker pull command.
--
--  # Pulling Image from Default Registry
--
--  >> docker pull redis
--  >> docker pull redis:alpine
--  >> docker pull fossa/haskell-static-alpine
--  >> docker pull fossa/haskell-static-alpine:latest
--  >> docker pull fossa/haskell-static-alpine@sha256:a89x...
--
--  # Pulling Image from Specific Registry
--
--  >> docker pull ghcr.io/fossas/haskell-dev-tools
--  >> docker pull ghcr.io/fossas/haskell-dev-tools:9.0.2
--  >> docker pull ghcr.io/fossas/haskell-dev-tools@sha256:a89x...
--
--  # Pulling Image from Specific Registry with Credentials
--
--  >> docker pull username:password@quay.io/repo/image:tag
--  >> docker pull https://username:password@quay.io/repo/image:tag
--
-- By default,
--
--  * Docker hub registry at "index.docker.io" is used, if registry
--    is not provided in the argument.
--
--  * Repository tag of "latest" is used, if repository reference is not
--    provided.
--
-- For more Information Refer to:
--  https://docs.docker.com/engine/reference/commandline/pull/
--
-- - -
module Container.Docker.SourceParser (
  RegistryImageSource (..),
  RegistryHostScheme (..),
  RepoReference (..),
  RepoTag (..),
  RepoDigest (..),
  parseImageUrl,
  defaultRegistry,
  defaultTag,
  defaultHttpScheme,
  suggestDockerExport,
  showReferenceWithSep,
  toRepoNameWithRegistry,
) where

import Control.Monad (unless, void)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.String.Conversion (ToString (toString), toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Prettyprinter (Doc, Pretty (pretty), indent, line, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Megaparsec (
  MonadParsec (try),
  Parsec,
  chunk,
  optional,
  some,
  (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

defaultRegistry :: Text
defaultRegistry = "index.docker.io" -- if you change this, update docs on top of module

defaultTag :: RepoReference
defaultTag = RepoReferenceTag . RepoTag $ "latest"

defaultHttpScheme :: RegistryHostScheme
defaultHttpScheme = RegistryHTTPS

data RepoReference
  = RepoReferenceTag RepoTag
  | RepoReferenceDigest RepoDigest
  deriving (Eq, Ord)

newtype RepoTag = RepoTag Text deriving (Show, Eq, Ord)
newtype RepoDigest = RepoDigest Text deriving (Show, Eq, Ord)

instance Show RepoReference where
  show (RepoReferenceTag (RepoTag tag)) = toString tag
  show (RepoReferenceDigest (RepoDigest digest)) = toString digest

data RegistryHostScheme
  = RegistryHTTP
  | RegistryHTTPS
  deriving (Eq, Ord)

instance Show RegistryHostScheme where
  show RegistryHTTP = "http://"
  show RegistryHTTPS = "https://"

data RegistryImageSource = RegistryImageSource
  { registryHost :: Text
  , registryScheme :: RegistryHostScheme
  , registryCred :: Maybe (Text, Text)
  , registryContainerRepository :: Text
  , registryContainerRepositoryReference :: RepoReference
  , platformArchitecture :: Text
  }
  deriving (Show, Eq, Ord)

instance Pretty RegistryImageSource where
  pretty (RegistryImageSource host scheme cred repo ref _) =
    pretty $
      (toText . show $ scheme)
        <> redactedCred
        <> host
        <> "/"
        <> repo
        <> showReferenceWithSep ref
    where
      redactedCred :: Text
      redactedCred = case cred of
        Nothing -> mempty
        _ -> "<REDACTED>:<REDACTED>@"

suggestDockerExport :: RegistryImageSource -> Doc AnsiStyle
suggestDockerExport (RegistryImageSource host _ _ repo ref _) =
  vsep
    [ "Try using exported container image for analysis instead."
    , line
    , indent 4 $
        vsep
          [ ">> " <> pullCmd
          , ">> " <> saveCmd
          , line
          , fossaAnalyzeCmd
          ]
    ]
  where
    imgIdentifier :: Text
    imgIdentifier = host <> "/" <> repo <> showReferenceWithSep ref

    pullCmd :: Doc AnsiStyle
    pullCmd = pretty $ "docker pull " <> imgIdentifier

    saveCmd :: Doc AnsiStyle
    saveCmd = pretty $ "docker save " <> imgIdentifier <> " > image-exported.tar"

    fossaAnalyzeCmd :: Doc AnsiStyle
    fossaAnalyzeCmd = "fossa container analyze image-exported.tar"

showReferenceWithSep :: RepoReference -> Text
showReferenceWithSep (RepoReferenceTag (RepoTag tag)) = ":" <> tag
showReferenceWithSep (RepoReferenceDigest (RepoDigest digest)) = "@" <> digest

-- | Parses to RegistryImageSource.
--
-- >> parse parseImageUrl "fossa/db" = https://index.docker.io/fossa/db:latest
-- >> parse parseImageUrl "fossa/db:alpine" = https://index.docker.io/fossa/db:alpine
-- >> parse parseImageUrl "quay.io/fossas/cli" = https://quay.io/fossas/cli:latest
-- >> parse parseImageUrl "quay.io/fossas/cli:9.0.0" = https://quay.io/fossas/cli:9.0.0
-- >> parse parseImageUrl "user:pass@quay.io/repo/cli:9.0.0" = https://user:pass@quay.io/repo/cli:9.0.0
-- -
parseImageUrl :: Text -> Parser RegistryImageSource
parseImageUrl targetArch = do
  imgSrc <-
    try (parseImageUrl' targetArch)
      <|> parseImageUrlWithDefaultRegistry defaultRegistry targetArch

  -- Docker Registry uses library/image for
  -- official images. For instance 'redis' should resolve to 'library/redis:latest'
  if isOfficialImage imgSrc
    then pure $ repoWithPrefix "library/" imgSrc
    else pure imgSrc
  where
    isOfficialImage :: RegistryImageSource -> Bool
    isOfficialImage imgSrc =
      registryHost imgSrc == defaultRegistry
        && not (Text.isInfixOf "/" $ registryContainerRepository imgSrc)

    repoWithPrefix :: Text -> RegistryImageSource -> RegistryImageSource
    repoWithPrefix prefix imgSrc =
      imgSrc
        { registryContainerRepository =
            prefix <> registryContainerRepository imgSrc
        }

toRepoNameWithRegistry :: RegistryImageSource -> Text
toRepoNameWithRegistry imgSrc =
  registryHost imgSrc
    <> "/"
    <> registryContainerRepository imgSrc

parseImageUrlWithDefaultRegistry :: Text -> Text -> Parser RegistryImageSource
parseImageUrlWithDefaultRegistry defaultHost targetArch = do
  (repo, repoRef) <- parseRepoRef
  pure $
    RegistryImageSource
      { registryHost = defaultHost
      , registryScheme = defaultHttpScheme
      , registryCred = Nothing
      , registryContainerRepository = repo
      , registryContainerRepositoryReference = repoRef
      , platformArchitecture = targetArch
      }

parseImageUrl' :: Text -> Parser RegistryImageSource
parseImageUrl' targetArch = do
  scheme <- optional parseHostScheme
  cred <- optional . try $ parseAuthCred
  host <- toText <$> some (alphaNumChar <|> char '.' <|> char '-')
  port :: Maybe Int <- (try . optional $ chunk ":" *> L.decimal)
  void $ optional (chunk "/")

  -- Avoid parsing org/repo:tag as valid image source
  -- as "org" is not a valid host url. But allows localhost.
  unless (Text.isInfixOf "." host || host == "localhost") $
    fail "Expected host to have '.' for domain"

  (repo, repoRef) <- parseRepoRef
  pure $
    RegistryImageSource
      { registryHost = host <> toText (maybe mempty (\p -> ":" <> show p) port)
      , registryScheme = fromMaybe defaultHttpScheme scheme
      , registryCred = cred
      , registryContainerRepository = repo
      , registryContainerRepositoryReference = repoRef
      , platformArchitecture = targetArch
      }

parseHostScheme :: Parser RegistryHostScheme
parseHostScheme =
  try (chunk "http://" $> RegistryHTTP)
    <|> (chunk "https://" $> RegistryHTTPS)

-- | Parses Authorization Credentials in URI.
--
-- >> parse parseAuthCred "a:b@" = ("a", "b")
-- -
parseAuthCred :: Parser (Text, Text)
parseAuthCred = do
  user <- toText <$> some alphaNumChar
  void (char ':')
  password <- toText <$> (some alphaNumChar)
  void (char '@')
  pure (user, password)

-- | Parses Repository name and reference.
--
-- >> parse parseRepoRef "redis" = ("redis", "latest")
-- >> parse parseRepoRef "redis:alpine" = ("redis", "alpine")
-- >> parse parseRepoRef "fossas/fossa:4.0.0" = ("fossas/fossa", "4.0.0")
-- >> parse parseRepoRef "fossas/fossa@4.0.0" = ("fossas/fossa", "4.0.0")
-- -
parseRepoRef :: Parser (Text, RepoReference)
parseRepoRef = try parseRepoRef' <|> parseRepoWithDefaultTag defaultTag

parseRepoRef' :: Parser (Text, RepoReference)
parseRepoRef' = do
  repo <- parseRepo
  ref <- parseRepoReference
  pure (repo, ref)

parseRepoReference :: Parser RepoReference
parseRepoReference = try parseRepoTag <|> parseRepoDigest

parseRepoTag :: Parser RepoReference
parseRepoTag = RepoReferenceTag . RepoTag <$> (chunk ":" *> parseRef)

parseRepoDigest :: Parser RepoReference
parseRepoDigest = RepoReferenceDigest . RepoDigest <$> (chunk "@" *> parseRef)

parseRepoWithDefaultTag :: RepoReference -> Parser (Text, RepoReference)
parseRepoWithDefaultTag tag = do
  repo <- parseRepo
  pure (repo, tag)

parseRepo :: Parser Text
parseRepo =
  toText
    <$> some (alphaNumChar <|> char '.' <|> char '_' <|> char '-' <|> char '/')

parseRef :: Parser Text
parseRef =
  toText
    <$> some (alphaNumChar <|> char '.' <|> char '_' <|> char '-' <|> char ':')
