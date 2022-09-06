use std::{io::Read, vec};

use byteorder::{BigEndian, ByteOrder, LittleEndian, ReadBytesExt};
use stable_eyre::{
    eyre::{ensure, Context},
    Result,
};

use super::header::Header;

pub struct Index(Vec<u16>);

impl Index {
    /// https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/constants.go#L10
    const ENTRY_SIZE: usize = 2;

    /// https://github.com/jssblck/go-rpmdb/blob/160242deff7a9ee82d1b493b62b7e50fd4c3e81c/pkg/bdb/hash_page.go#L102
    const PAIR_SIZE: usize = Self::ENTRY_SIZE * 2;

    /// Read [`Self`] out of a file in plain byte order.
    /// This can't be a `ByteParser` because it takes an additional argument and a different data type than `Read`.
    ///
    /// Reference: https://github.com/jssblck/go-rpmdb/blob/160242deff7a9ee82d1b493b62b7e50fd4c3e81c/pkg/bdb/hash_page.go#L89
    pub fn parse<E: ByteOrder>(data: &[u8], entries: usize) -> Result<Self> {
        ensure!(
            entries % 2 == 0,
            "entries must only be requested in pairs: {entries}"
        );

        // Every entry is a 2-byte offset that points somewhere in the current database page.
        let data_len = Self::size(entries);
        let index_data = data
            .iter()
            .skip(Header::SIZE)
            .take(data_len)
            .cloned()
            .collect::<Vec<_>>();
        ensure!(
            index_data.len() == data_len,
            "short read: expected {data_len}, got {}",
            index_data.len()
        );

        // data is stored in key-value pairs (each a `u16`), but we only care about the values.
        // Reference: https://github.com/jssblck/go-rpmdb/blob/160242deff7a9ee82d1b493b62b7e50fd4c3e81c/pkg/bdb/hash_page.go#L100-L108
        index_data
            .chunks(Self::PAIR_SIZE)
            .map(|chunk| parse_pair_u16::<E>(chunk).map(snd))
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

    /// Given a parsed index, report its length.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl IntoIterator for Index {
    type Item = u16;

    type IntoIter = vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

fn parse_pair_u16<E: ByteOrder>(mut r: impl Read) -> Result<(u16, u16)> {
    let k = r.read_u16::<E>().context("read pair 0 index")?;
    let v = r.read_u16::<E>().context("read pair 1 index")?;
    Ok((k, v))
}

fn snd<T, K>((_, t): (K, T)) -> T {
    t
}
