use std::io::Read;

use byteorder::{ByteOrder, ReadBytesExt};
use getset::CopyGetters;
use stable_eyre::Result;

use crate::parse::{read_n, ByteParser};

/// An entry in a page.
///
/// Reference: https://github.com/jssblck/go-rpmdb/blob/160242deff7a9ee82d1b493b62b7e50fd4c3e81c/pkg/bdb/hash_page_entry.go#L11
#[derive(Debug, Default, PartialEq, Eq, CopyGetters)]
#[get_copy = "pub"]
pub struct Entry {
    page_type: u8,    /*     0: Page type. */
    _unused: [u8; 3], /* 01-03: Padding, unused. */
    page_no: u32,     /* 04-07: Offpage page number. */
    length: u32,      /* 08-11: Total length of item. */
}

impl Entry {
    /// https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/constants.go#L24
    pub const SIZE: usize = 12;
}

impl ByteParser for Entry {
    type Output = Self;

    /// Read [`Self`] out of a file in plain byte order.
    ///
    /// Reference: https://github.com/jssblck/go-rpmdb/blob/160242deff7a9ee82d1b493b62b7e50fd4c3e81c/pkg/bdb/hash_page_entry.go#L18
    fn parse<E: ByteOrder>(r: &mut impl Read) -> Result<Self::Output> {
        Ok(Self {
            page_type: r.read_u8()?,
            _unused: read_n(r)?,
            page_no: r.read_u32::<E>()?,
            length: r.read_u32::<E>()?,
        })
    }
}
