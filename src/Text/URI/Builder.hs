{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Text.URI.Builder
  ( Query (..),
    PathComponent (..),
    TrailingSlash (..),
    renderPath,
    setPath,
    setQuery,
  )
where

import Control.Effect.Diagnostics
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text
import Text.URI

newtype PathComponent = PathComponent {unPathComponent :: Text} deriving (Eq, Ord, Show)

newtype TrailingSlash = TrailingSlash {unTrailingSlash :: Bool} deriving (Eq, Ord, Show)

type URIPath = Maybe (Bool, NonEmpty (RText 'PathPiece))

setPath :: Has Diagnostics sig m => [PathComponent] -> TrailingSlash -> URI -> m URI
setPath pcomlist slash uri = do
  newpath <- convertPaths pcomlist slash
  pure $ uri {uriPath = newpath}

convertPaths :: Has Diagnostics sig m => [PathComponent] -> TrailingSlash -> m URIPath
convertPaths [] _ = pure Nothing
convertPaths (path : paths) (TrailingSlash slash) = do
  pathstream <- fromEither $ traverse (mkPathPiece . unPathComponent) (path :| paths)
  pure $ Just (slash, pathstream)

renderPath :: Has Diagnostics sig m => [PathComponent] -> TrailingSlash -> m Text
renderPath paths slash = render <$> setPath paths slash uri
  where
    uri = emptyURI {uriAuthority = Left True}

data Query
  = Flag Text
  | Pair Text Text
  deriving (Eq, Ord, Show)

setQuery :: Has Diagnostics sig m => [Query] -> URI -> m URI
setQuery qlist uri = do
  let xform :: Has Diagnostics sig m => Query -> m QueryParam
      xform =
        fromEither . \case
          Flag f -> QueryFlag <$> mkQueryKey f
          Pair k v -> QueryParam <$> mkQueryKey k <*> mkQueryValue v

  qplist <- traverse xform qlist
  pure $ uri {uriQuery = qplist}
