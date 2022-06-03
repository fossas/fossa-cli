{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Report.Attribution (
  Attribution (..),
  Dependency (..),
  License (..),
  LicenseContents (..),
  LicenseCopyright (..),
  LicenseDetails (..),
  LicenseName (..),
  Project (..),
) where

import Data.Aeson
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text)

newtype LicenseName = LicenseName {rawName :: Text}
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

newtype LicenseContents = LicenseContents {rawContents :: Text}
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

newtype LicenseCopyright = LicenseCopyright {rawCopyright :: Text}
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

data LicenseDetails = LicenseDetails
  { licenseText :: LicenseContents
  , licenseCopyrights :: [LicenseCopyright]
  }
  deriving (Eq, Ord, Show)

data Attribution = Attribution
  { attribProject :: Project
  , attribDirectDeps :: [Dependency]
  , attribDeepDeps :: [Dependency]
  , attribLicenses :: Map LicenseName LicenseContents
  , attribLicenseDetails :: Maybe (Map LicenseName LicenseDetails)
  }
  deriving (Eq, Show, Ord)

data Dependency = Dependency
  { depPackage :: Text
  , depSource :: Text
  , depVersion :: Maybe Text
  , depIsGolang :: Maybe Bool
  , depHash :: Maybe Text
  , depAuthors :: [Text]
  , depDescription :: Maybe Text
  , depLicenses :: Maybe [License]
  , depOtherLicenses :: [License]
  , depProjectUrl :: Maybe Text
  , depDependencyPaths :: [Text]
  , depNotes :: [Text]
  , depDownloadUrl :: Maybe Text
  , depTitle :: Text
  }
  deriving (Eq, Show, Ord)

data License = License
  { licenseName :: LicenseName
  , licenseAttribution :: Maybe LicenseContents
  }
  deriving (Eq, Show, Ord)

data Project = Project
  { projectName :: Text
  , projectRevision :: Text
  }
  deriving (Eq, Show, Ord)

instance FromJSON Attribution where
  parseJSON = withObject "Attribution" $ \obj ->
    Attribution
      <$> obj .: "project"
      <*> obj .:? "directDependencies" .!= []
      <*> obj .:? "deepDependencies" .!= []
      <*> obj .: "licenses"
      <*> obj .:? "licenseDetails"

instance ToJSON Attribution where
  toJSON Attribution{..} =
    object $
      [ "project" .= attribProject
      , "directDependencies" .= attribDirectDeps
      , "deepDependencies" .= attribDeepDeps
      , "licenses" .= attribLicenses
      ]
        ++ if isNothing attribLicenseDetails
          then []
          else ["licenseDetails" .= attribLicenseDetails]

instance FromJSON Dependency where
  parseJSON = withObject "Dependency" $ \obj ->
    Dependency
      <$> obj .: "package"
      <*> obj .: "source"
      <*> obj .:? "version"
      <*> obj .:? "isGolang"
      <*> obj .:? "hash"
      <*> (catMaybes <$> obj .: "authors")
      <*> obj .:? "description"
      <*> obj .:? "licenses"
      <*> obj .: "otherLicenses"
      <*> obj .:? "projectUrl"
      <*> obj .: "dependencyPaths"
      <*> (catMaybes <$> obj .: "notes")
      <*> obj .:? "downloadUrl"
      <*> obj .: "title"

instance ToJSON Dependency where
  toJSON Dependency{..} =
    object
      [ "package" .= depPackage
      , "source" .= depSource
      , "version" .= depVersion
      , "isGolang" .= depIsGolang
      , "hash" .= depHash
      , "authors" .= depAuthors
      , "description" .= depDescription
      , "licenses" .= depLicenses
      , "otherLicenses" .= depOtherLicenses
      , "projectUrl" .= depProjectUrl
      , "dependencyPaths" .= depDependencyPaths
      , "notes" .= depNotes
      , "downloadUrl" .= depDownloadUrl
      , "title" .= depTitle
      ]

instance FromJSON License where
  parseJSON = withObject "License" $ \obj ->
    License
      <$> obj .: "name"
      <*> obj .:? "attribution"

instance ToJSON License where
  toJSON License{..} =
    object
      [ "name" .= licenseName
      , "attribution" .= licenseAttribution
      ]

instance FromJSON Project where
  parseJSON = withObject "Project" $ \obj ->
    Project
      <$> obj .: "name"
      <*> obj .: "revision"

instance ToJSON Project where
  toJSON Project{..} =
    object
      [ "name" .= projectName
      , "revision" .= projectRevision
      ]

instance FromJSON LicenseDetails where
  parseJSON = withObject "LicenseDetails" $ \obj ->
    LicenseDetails
      <$> obj .: "text"
      <*> obj .: "copyrights"

instance ToJSON LicenseDetails where
  toJSON LicenseDetails{..} =
    object
      [ "text" .= licenseText
      , "copyrights" .= licenseCopyrights
      ]
