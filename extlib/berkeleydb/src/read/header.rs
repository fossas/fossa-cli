use std::io::{Cursor, Read};

use byteorder::{BigEndian, ByteOrder, LittleEndian, ReadBytesExt};
use getset::CopyGetters;
use stable_eyre::Result;

use crate::read_n;

#[derive(Debug, Default, PartialEq, Eq, CopyGetters)]
#[get_copy = "pub"]
pub struct Header {
    lsn: [u8; 8],
    page_no: u32,
    previous_page_no: u32,
    next_page_no: u32,
    num_entries: u16,
    free_area_offset: u16,
    tree_level: u8,
    page_type: u8,
}

impl Header {
    /// All pages have the same header size.
    pub const SIZE: usize = 26;

    const UNSORTED_PAGE_TYPE: u8 = 2;
    const SORTED_PAGE_TYPE: u8 = 13;

    /// Read [`Self`] out of a file in plain byte order.
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
    pub fn should_skip(&self) -> bool {
        self.page_type != Self::UNSORTED_PAGE_TYPE && self.page_type != Self::SORTED_PAGE_TYPE
    }
}
