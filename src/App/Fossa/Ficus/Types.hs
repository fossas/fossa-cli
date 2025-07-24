{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Ficus.Types (
    FicusResults(..)
) where

data FicusResults = FicusResults { ficusAnalysisId :: Int }
