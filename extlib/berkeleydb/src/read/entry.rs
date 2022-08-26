use std::io::Read;

use byteorder::{ByteOrder, ReadBytesExt};
use getset::CopyGetters;
use stable_eyre::Result;

use crate::parse::{read_n, ByteParser};

/// An entry in a page.
///
/// Source: https://github.com/berkeleydb/libdb/blob/5b7b02ae052442626af54c176335b67ecc613a30/src/dbinc/db_page.h#L655
#[derive(Debug, Default, PartialEq, Eq, CopyGetters)]
#[get_copy = "pub"]
pub struct Entry {
    page_type: u8,    /*     0: Page type. */
    _unused: [u8; 3], /* 01-03: Padding, unused. */
    page_no: u32,     /* 04-07: Offpage page number. */
    length: u32,      /* 08-11: Total length of item. */
}

impl Entry {
    pub const SIZE: usize = 12;
}

impl ByteParser for Entry {
    type Output = Self;

    /// Read [`Self`] out of a file in plain byte order.
    fn parse<E: ByteOrder>(r: &mut impl Read) -> Result<Self::Output> {
        Ok(Self {
            page_type: r.read_u8()?,
            _unused: read_n(r)?,
            page_no: r.read_u32::<E>()?,
            length: r.read_u32::<E>()?,
        })
    }
}
