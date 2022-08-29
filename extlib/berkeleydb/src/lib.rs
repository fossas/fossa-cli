//! Parse the BerkeleyDB container format.
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

use std::{
    fs::File,
    io::{BufReader, Seek, SeekFrom},
    path::PathBuf,
};

use log::debug;
use metadata::{Generic, Hash};
use parse::ByteParser;
use stable_eyre::{eyre::Context, Result};

pub mod metadata;
mod parse;
pub mod read;

/// Contains the reference to the BerkeleyDB file and its metadata.
///
/// Reference: https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/bdb.go#L22
pub struct BerkeleyDB {
    file: BufReader<File>,
    pub metadata: metadata::Page,
}

impl BerkeleyDB {
    /// Open the database and parse its metadata, which is used for future reading.
    pub fn open(path: &PathBuf) -> Result<BerkeleyDB> {
        debug!("📂 Open DB: {path:?}");
        let file = File::open(&path).context("open file")?;

        // Data structure parsers rely on many read calls for small numbers of bytes at once.
        // We use a bufreader so that we don't trigger syscalls with all these.
        let mut file = BufReader::new(file);

        let (generic, big_endian) = Generic::parse(&mut file).context("parse generic metadata")?;
        let hash = Hash::parse_dyn(&mut file, big_endian).context("parse hash metadata")?;

        let metadata = metadata::Page::builder()
            .generic(generic)
            .hash(hash)
            .big_endian(big_endian)
            .build();

        debug!("🔖 Parsed metadata: {metadata:?}");
        metadata.validate().context("validate metadata")?;
        file.seek(SeekFrom::Start(0)).context("seek to start")?;

        debug!("✅ Metadata validated and cursor reset; db is ready for reading!");
        Ok(BerkeleyDB { file, metadata })
    }
}
