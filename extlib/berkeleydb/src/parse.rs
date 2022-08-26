use std::io::Read;

use byteorder::{BigEndian, ByteOrder, LittleEndian};
use stable_eyre::Result;

/// Not all parser types make use of this trait, but for those that can it saves a little duplication.
pub trait ByteParser {
    type Output;

    /// Read [`Self`] out of a file in plain byte order.
    fn parse<E: ByteOrder>(r: &mut impl Read) -> Result<Self::Output>;

    /// Like [`Self::parse`], but with dynamic endianness.
    fn parse_dyn(r: &mut impl Read, big_endian: bool) -> Result<Self::Output> {
        if big_endian {
            Self::parse::<BigEndian>(r)
        } else {
            Self::parse::<LittleEndian>(r)
        }
    }
}

/// Read exactly `n` bytes from a reader into a constant sized array.
pub(crate) fn read_n<const N: usize>(r: &mut impl Read) -> Result<[u8; N]> {
    let mut buf = [0; N];
    r.read_exact(&mut buf)?;
    Ok(buf)
}

/// Read exactly `n` bytes from a reader into a variably sized vec.
pub(crate) fn slice(r: &mut impl Read, n: usize) -> Result<Vec<u8>> {
    let mut buf = vec![0; n];
    r.read_exact(&mut buf)?;
    Ok(buf)
}
