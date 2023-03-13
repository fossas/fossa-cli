{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.EmbeddedBinary.Wiggins (embeddedBinaryWiggins) where

import Data.ByteString (ByteString)
import Data.FileEmbed.Extra (embedFileIfExists)

embeddedBinaryWiggins :: ByteString
embeddedBinaryWiggins = $(embedFileIfExists "vendor-bins/wiggins")
