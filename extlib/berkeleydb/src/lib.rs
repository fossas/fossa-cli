use std::{
    fs::File,
    io::{BufReader, Seek, SeekFrom},
    path::PathBuf,
};

use byteorder::{BigEndian, LittleEndian};
use metadata::{Generic, Hash, Page};
use stable_eyre::{eyre::Context, Result};

pub mod metadata;

pub struct BerkeleyDB {
    file: BufReader<File>,
    pub metadata: Page,
}

pub fn open(path: PathBuf) -> Result<BerkeleyDB> {
    let file = File::open(path)?;
    let mut file = BufReader::new(file);

    let generic = Generic::parse::<LittleEndian>(&mut file)?;
    let (generic, big_endian) = if generic.is_big_endian() {
        file.seek(SeekFrom::Start(0))?;
        (Generic::parse::<BigEndian>(&mut file)?, true)
    } else {
        (generic, false)
    };

    let hash = if big_endian {
        Hash::parse::<BigEndian>(&mut file)?
    } else {
        Hash::parse::<LittleEndian>(&mut file)?
    };

    let metadata = Page {
        generic,
        hash,
        big_endian,
    };

    metadata.validate().context("validate page metadata")?;
    file.seek(SeekFrom::Start(0))?;
    Ok(BerkeleyDB { file, metadata })
}
