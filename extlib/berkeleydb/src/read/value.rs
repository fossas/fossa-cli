use std::io::{Seek, SeekFrom};

use byteorder::{BigEndian, ByteOrder, LittleEndian};
use serde::{Serialize, Serializer};
use stable_eyre::{
    eyre::{bail, ensure, Context},
    Result,
};

use crate::{
    parse::{slice_dyn, ByteParser},
    read::{header::Header, Entry},
    BerkeleyDB,
};

/// Values are raw bytes.
/// `fossa-cli` contains a parser for these values, because the same blob format is shared between all RPM backends.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Value(Vec<u8>);

/// Serialize the value as a `base64` string.
impl Serialize for Value {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let encoded = base64::encode(&self.0);
        serializer.serialize_str(&encoded)
    }
}

impl Value {
    /// Hash Offset pages are the only ones that contain data.
    /// https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/bdb.go#L110-L113
    pub const SUPPORTED_PAGE_TYPE: u8 = 3;

    /// https://github.com/berkeleydb/libdb/blob/v5.3.28/src/dbinc/db_page.h#L35-L53
    pub const OVERFLOW_PAGE_TYPE: u8 = 7;

    /// Read [`Self`] out of a DB and page with the provided page offset.
    ///
    /// Reference: https://github.com/jssblck/go-rpmdb/blob/160242deff7a9ee82d1b493b62b7e50fd4c3e81c/pkg/bdb/hash_page.go#L34
    pub fn parse<E: ByteOrder>(db: &mut BerkeleyDB, page: &[u8], index: u16) -> Result<Value> {
        let index = usize::from(index);

        // Use `.get` instead of index so we can't cause a panic.
        let page_type = if let Some(page_type) = page.get(index) {
            page_type.to_owned()
        } else {
            bail!("block {index} did not exist in page, but was pointed at by index");
        };

        // Other pages don't contain data.
        // https://github.com/jssblck/go-rpmdb/blob/160242deff7a9ee82d1b493b62b7e50fd4c3e81c/pkg/bdb/hash_page.go#L38-L41
        ensure!(
            page_type == Self::SUPPORTED_PAGE_TYPE,
            "unsupported page type: {page_type}"
        );

        // Use iterator APIs instead of direct slicing so that we can't cause a panic.
        // It's a little slower, but worth it for a better error message.
        let entry_data = page
            .iter()
            .skip(index)
            .take(Entry::SIZE)
            .cloned()
            .collect::<Vec<_>>();
        let entry = Entry::parse::<E>(&mut entry_data.as_slice()).context("parse entry")?;

        // The value may be spread over multiple pages. Build it all together.
        // Reference: https://github.com/jssblck/go-rpmdb/blob/160242deff7a9ee82d1b493b62b7e50fd4c3e81c/pkg/bdb/hash_page.go#L52-L84
        let mut value = Vec::new();
        let mut current_page_no = entry.page_no();
        while current_page_no != 0 {
            let page_size = db.metadata.generic().page_size();
            let page_start = page_size * current_page_no;
            db.file
                .seek(SeekFrom::Start(page_start.into()))
                .context("seek to page")?;

            let page_size = page_size.try_into().context("convert page size")?;
            let page = slice_dyn(&mut db.file, page_size).context("read page from file")?;

            let header = Header::parse::<E>(&mut page.as_slice()).context("parse header")?;
            if header.page_type() != Self::OVERFLOW_PAGE_TYPE {
                current_page_no = header.next_page_no();
                continue;
            }

            if header.has_next_page() {
                value.extend(page.iter().skip(Header::SIZE).cloned());
            } else {
                value.extend(
                    page.iter()
                        .skip(Header::SIZE)
                        .take(header.free_area_offset().into())
                        .cloned(),
                );
            }

            current_page_no = header.next_page_no();
        }

        Ok(Value(value))
    }

    /// Like [`Self::parse`], but with dynamic endianness.
    pub fn parse_dyn(db: &mut BerkeleyDB, page: &[u8], index: u16) -> Result<Self> {
        if db.metadata.big_endian() {
            Self::parse::<BigEndian>(db, page, index)
        } else {
            Self::parse::<LittleEndian>(db, page, index)
        }
    }

    /// Used for tests.
    pub fn into_inner(self) -> Vec<u8> {
        self.0
    }
}
