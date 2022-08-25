use std::io::{Cursor, Read, Seek, SeekFrom};

use byteorder::{BigEndian, ByteOrder, LittleEndian};
use stable_eyre::{
    eyre::{bail, eyre, Context},
    Result,
};

use crate::{
    read::{header::Header, Entry},
    read_n_dyn,
};

pub struct Value(Vec<u8>);

impl Value {
    /// Hash Offset pages are supported: https://github.com/berkeleydb/libdb/blob/v5.3.28/src/dbinc/db_page.h#L569-L573
    pub const SUPPORTED_PAGE_TYPE: u8 = 3;

    /// https://github.com/berkeleydb/libdb/blob/v5.3.28/src/dbinc/db_page.h#L35-L53
    pub const OVERFLOW_PAGE_TYPE: u8 = 7;

    pub fn parse<E: ByteOrder, F: Read + Seek>(
        db: &mut F,
        page: &[u8],
        index: u16,
        page_size: u32,
    ) -> Result<Value> {
        let index = usize::from(index);
        let page_type = page
            .get(index)
            .ok_or_else(|| {
                eyre!("block {index} did not exist in page, but was pointed at by index")
            })?
            .to_owned();

        if page_type != Self::SUPPORTED_PAGE_TYPE {
            bail!("unsupported page type: {page_type}");
        }

        let entry = Entry::parse::<E>(&mut Cursor::new(page)).context("parse entry")?;

        let mut hash_value = Vec::new();
        let mut current_page_no = entry.page_no();
        while current_page_no != 0 {
            let page_start = page_size * current_page_no;
            db.seek(SeekFrom::Start(page_start.into()))
                .context("seek to page")?;

            let page_size = page_size.try_into().context("convert page size")?;
            let page = read_n_dyn(db, page_size).context("read page from file")?;
            let page = page.as_slice();

            let header = Header::parse::<E>(&mut Cursor::new(page)).context("parse header")?;
            if header.page_type() != Self::OVERFLOW_PAGE_TYPE {
                current_page_no = header.next_page_no();
                continue;
            }

            if header.next_page_no() == 0 {
                hash_value.extend(
                    page.iter()
                        .skip(Header::SIZE)
                        .take(header.free_area_offset().into())
                        .cloned(),
                );
            } else {
                hash_value.extend(page.iter().skip(Header::SIZE).cloned());
            }

            current_page_no = header.next_page_no();
        }

        Ok(Value(hash_value))
    }

    /// Like [`Self::parse`], but with dynamic endianness.
    pub fn parse_dyn<F: Read + Seek>(
        db: &mut F,
        page: &[u8],
        index: u16,
        page_size: u32,
        big_endian: bool,
    ) -> Result<Self> {
        if big_endian {
            Self::parse::<BigEndian, F>(db, page, index, page_size)
        } else {
            Self::parse::<LittleEndian, F>(db, page, index, page_size)
        }
    }
}
