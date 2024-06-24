-- |
-- Module : Observation
-- Description : Output observations from the CLI
--
-- This module is intended to be the output format accepted by Sparkle.
module Data.BuildOutput (BuildOutput (..), Observation (..)) where

import App.Fossa.Container.Sources.JarAnalysis
import Data.Aeson
import GHC.Generics (Generic)
import Srclib.Types (SourceUnit)

newtype Observation
  = JarsInContainer JarObservation
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Observation where
  toEncoding = genericToEncoding defaultOptions{sumEncoding = UntaggedValue}

data BuildOutput = BuildOutput
  { observations :: [Observation]
  , sourceUnits :: [SourceUnit]
  }

instance ToJSON BuildOutput where
  toJSON BuildOutput{observations, sourceUnits} =
    object
      [ "observations" .= observations
      , "sourceUnits" .= sourceUnits
      ]
