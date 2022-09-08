//! Parse the BerkeleyDB container format.
//!
//! # C Interop
//!
//! This library is also built as a C binary for FFI.
//! Refer to `ffi` for more information.
//!
//! # Logic decisions
//!
//! This library is heavily based on `go-rpmdb`: https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/bdb.go
//! (I forked this to make some minor modifications for testing).
//! For the most part this library is a direct port with some modification to fit our use case.
//!
//! In turn, that library primarily uses the C implementation for guidance.
//! The `go-rpmdb` library then makes some logic choices (such as which page types are ignored)
//! which we've simply accepted and ported, as they generally didn't include context on _why_ those decisions were made.
//!
//! # Parsers
//!
//! The Go implementation uses the Go `encoding/binary` package to read bytes directly into structs.
//! Rather than try to reimplement the nuances of that library (most of which are Go specific),
//! this library uses simple readers to read bytes directly into the types required for the struct.

use std::io::{Cursor, Read, Seek, SeekFrom};

use log::debug;
use metadata::{Generic, Hash};
use parse::ByteParser;
use stable_eyre::{eyre::Context, Result};

mod ffi;
pub mod metadata;
mod parse;
pub mod read;

pub use ffi::*;

/// Contains the reference to the BerkeleyDB file and its metadata.
///
/// Reference: https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/bdb.go#L22
pub struct BerkeleyDB {
    file: Cursor<Vec<u8>>,
    pub metadata: metadata::Page,
}

impl BerkeleyDB {
    /// Parse a BerkeleyDB database from the provided slice.
    pub fn from(input: Vec<u8>) -> Result<Self> {
        let mut file = Cursor::new(input);

        let metadata = BerkeleyDB::parse_metadata(&mut file).context("parse metadata")?;
        debug!("🔖 Parsed metadata: {metadata:?}");

        file.seek(SeekFrom::Start(0)).context("seek file start")?;
        debug!("✅ Metadata validated and cursor reset; db is ready for reading!");
        Ok(BerkeleyDB { file, metadata })
    }

    fn parse_metadata<F: Read + Seek>(file: &mut F) -> Result<metadata::Page> {
        let (generic, big_endian) = Generic::parse(file).context("generic metadata")?;
        let hash = Hash::parse_dyn(file, big_endian).context("hash metadata")?;

        let metadata = metadata::Page::builder()
            .generic(generic)
            .hash(hash)
            .big_endian(big_endian)
            .build();

        metadata.validate().context("validate")?;
        Ok(metadata)
    }
}
