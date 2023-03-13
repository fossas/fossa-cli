{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.EmbeddedBinary.Themis (embeddedBinaryThemis
                                       , embeddedBinaryThemisIndex) where

import Data.ByteString (ByteString)
import Data.FileEmbed.Extra (embedFileIfExists)

embeddedBinaryThemis :: ByteString
embeddedBinaryThemis = $(embedFileIfExists   "vendor-bins/themis-cli")

embeddedBinaryThemisIndex :: ByteString
embeddedBinaryThemisIndex = $(embedFileIfExists "vendor-bins/index.gob.xz")
