use std::io::{Cursor, Read};

use byteorder::{BigEndian, ByteOrder, LittleEndian, ReadBytesExt};
use getset::CopyGetters;
use stable_eyre::Result;

use crate::parse::read_n;

/// The header for a hash page.
///
/// Reference: https://github.com/jssblck/go-rpmdb/blob/160242deff7a9ee82d1b493b62b7e50fd4c3e81c/pkg/bdb/hash_page.go#L13
#[derive(Debug, Default, PartialEq, Eq, CopyGetters)]
#[get_copy = "pub"]
pub struct Header {
    lsn: [u8; 8],          /* 00-07: LSN. */
    page_no: u32,          /* 08-11: Current page number. */
    previous_page_no: u32, /* 12-15: Previous page number. */
    next_page_no: u32,     /* 16-19: Next page number. */
    num_entries: u16,      /* 20-21: Number of items on the page. */
    free_area_offset: u16, /* 22-23: High free byte page offset. */
    tree_level: u8,        /*    24: Btree tree level. */
    page_type: u8,         /*    25: Page type. */
}

impl Header {
    /// All pages have the same header size.
    /// https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/constants.go#L12
    pub const SIZE: usize = 26;

    /// https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/constants.go#L16
    const UNSORTED_PAGE_TYPE: u8 = 2;

    /// https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/constants.go#L19
    const SORTED_PAGE_TYPE: u8 = 13;

    /// Read [`Self`] out of a file in plain byte order.
    ///
    /// Reference: https://github.com/jssblck/go-rpmdb/blob/160242deff7a9ee82d1b493b62b7e50fd4c3e81c/pkg/bdb/hash_page.go#L24
    pub fn parse<E: ByteOrder>(r: &mut impl Read) -> Result<Self> {
        Ok(Self {
            lsn: read_n(r)?,
            page_no: r.read_u32::<E>()?,
            previous_page_no: r.read_u32::<E>()?,
            next_page_no: r.read_u32::<E>()?,
            num_entries: r.read_u16::<E>()?,
            free_area_offset: r.read_u16::<E>()?,
            tree_level: r.read_u8()?,
            page_type: r.read_u8()?,
        })
    }

    /// Like [`Self::parse`], but with dynamic endianness.
    pub fn parse_dyn(r: &[u8], big_endian: bool) -> Result<Self> {
        let mut r = Cursor::new(r);
        if big_endian {
            Self::parse::<BigEndian>(&mut r)
        } else {
            Self::parse::<LittleEndian>(&mut r)
        }
    }

    /// The parser only cares about some page types.
    ///
    /// Reference: https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/bdb.go#L92-L96
    pub fn should_skip(&self) -> bool {
        self.page_type != Self::UNSORTED_PAGE_TYPE && self.page_type != Self::SORTED_PAGE_TYPE
    }

    /// Whether the header points to a next page.
    pub fn has_next_page(&self) -> bool {
        self.next_page_no != 0
    }
}
