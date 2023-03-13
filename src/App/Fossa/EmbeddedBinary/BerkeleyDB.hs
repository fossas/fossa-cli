{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.EmbeddedBinary.BerkeleyDB (embeddedBinaryBerkeleyDB) where

import Data.ByteString (ByteString)
import Data.FileEmbed.Extra (embedFileIfExists)

-- To build this, run `make build` or `cargo build --release`.
#ifdef mingw32_HOST_OS
embeddedBinaryBerkeleyDB :: ByteString
embeddedBinaryBerkeleyDB = $(embedFileIfExists "target/release/berkeleydb.exe")
#else
embeddedBinaryBerkeleyDB :: ByteString
embeddedBinaryBerkeleyDB = $(embedFileIfExists "target/release/berkeleydb")
#endif
