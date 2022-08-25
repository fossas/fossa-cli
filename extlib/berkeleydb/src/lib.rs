use std::{
    fs::File,
    io::{BufReader, Read, Seek, SeekFrom},
    path::PathBuf,
};

use byteorder::{BigEndian, LittleEndian};
use read::{value::Value, EntryReader};
use stable_eyre::{eyre::Context, Result};

pub mod metadata;
mod read;

pub struct BerkeleyDB {
    file: BufReader<File>,
    pub metadata: metadata::Page,
}

impl BerkeleyDB {
    pub fn open(path: PathBuf) -> Result<BerkeleyDB> {
        let file = File::open(&path).wrap_err_with(|| format!("open file at {path:?}"))?;
        let mut file = BufReader::new(file);

        let (generic, big_endian) = parse_generic(&mut file).context("parse generic metadata")?;
        let hash = parse_hash(&mut file, big_endian).context("parse hash metadata")?;

        let metadata = metadata::Page::builder()
            .generic(generic)
            .hash(hash)
            .big_endian(big_endian)
            .build();
        metadata.validate().context("validate page metadata")?;

        file.seek(SeekFrom::Start(0)).context("seek to start")?;
        Ok(BerkeleyDB { file, metadata })
    }

    pub fn read(self) -> Result<impl Iterator<Item = Result<Value>>> {
        EntryReader::new(self).context("construct entry reader")
    }
}

fn parse_generic(file: &mut BufReader<File>) -> Result<(metadata::Generic, bool)> {
    let generic = metadata::Generic::parse::<LittleEndian>(file).context("little endian")?;
    if generic.is_big_endian() {
        file.seek(SeekFrom::Start(0)).context("seek to start")?;
        let generic = metadata::Generic::parse::<BigEndian>(file).context("big endian")?;
        Ok((generic, true))
    } else {
        Ok((generic, false))
    }
}

fn parse_hash(file: &mut BufReader<File>, big_endian: bool) -> Result<metadata::Hash> {
    if big_endian {
        metadata::Hash::parse::<BigEndian>(file).context("big endian")
    } else {
        metadata::Hash::parse::<LittleEndian>(file).context("little endian")
    }
}

pub(crate) fn read_n<const N: usize>(r: &mut impl Read) -> Result<[u8; N]> {
    let mut buf = [0; N];
    r.read_exact(&mut buf)?;
    Ok(buf)
}

pub(crate) fn read_n_dyn(r: &mut impl Read, n: usize) -> Result<Vec<u8>> {
    let mut buf = vec![0; n];
    r.read_exact(&mut buf)?;
    Ok(buf)
}
