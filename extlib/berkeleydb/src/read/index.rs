use std::{io::Read, vec};

use byteorder::{BigEndian, ByteOrder, LittleEndian, ReadBytesExt};
use stable_eyre::{
    eyre::{bail, Context},
    Result,
};

use super::header::Header;

pub struct Index(Vec<u16>);

impl Index {
    const ENTRY_SIZE: usize = 2;
    const PAIR_SIZE: usize = Self::ENTRY_SIZE * 2;

    /// Read [`Self`] out of a file in plain byte order.
    pub fn parse<E: ByteOrder>(data: &[u8], entries: usize) -> Result<Self> {
        if entries % 2 != 0 {
            bail!("entries must only be requested in pairs: {entries}");
        }

        // Every entry is a 2-byte offset that points somewhere in the current database page.
        let data_len = Self::size(entries);
        let index_data = data
            .iter()
            .skip(Header::SIZE)
            .take(data_len)
            .cloned()
            .collect::<Vec<_>>();
        if index_data.len() != data_len {
            bail!("short read: expected {data_len}, got {}", index_data.len());
        }

        // data is stored in key-value pairs (https://github.com/berkeleydb/libdb/blob/5b7b02ae052442626af54c176335b67ecc613a30/src/dbinc/db_page.h#L591)
        // skip over keys and only keep values
        index_data
            .chunks(Self::PAIR_SIZE)
            .map(|chunk| to_pair_u16::<E>(chunk).map(snd))
            .collect::<Result<Vec<_>>>()
            .map(Index)
            .context("read index values")
    }

    /// Like [`Self::parse`], but with dynamic endianness.
    pub fn parse_dyn(data: &[u8], entries: usize, big_endian: bool) -> Result<Self> {
        if big_endian {
            Self::parse::<BigEndian>(data, entries).context("big endian")
        } else {
            Self::parse::<LittleEndian>(data, entries).context("little endian")
        }
    }

    /// Given a number of expected entries, returns the total size for all the entries.
    fn size(entries: usize) -> usize {
        entries * Self::ENTRY_SIZE
    }
}

impl IntoIterator for Index {
    type Item = u16;

    type IntoIter = vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

fn to_pair_u16<E: ByteOrder>(mut r: impl Read) -> Result<(u16, u16)> {
    let k = r.read_u16::<E>().context("read pair 0 index")?;
    let v = r.read_u16::<E>().context("read pair 1 index")?;
    Ok((k, v))
}

fn snd<T>((_, t): (T, T)) -> T {
    t
}
