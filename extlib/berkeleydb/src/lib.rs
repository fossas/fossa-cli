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

pub struct BerkeleyDB {
    file: BufReader<File>,
    pub metadata: metadata::Page,
}

impl BerkeleyDB {
    pub fn open(path: &PathBuf) -> Result<BerkeleyDB> {
        debug!("📂 Open DB: {path:?}");
        let file = File::open(&path).context("open file")?;
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
