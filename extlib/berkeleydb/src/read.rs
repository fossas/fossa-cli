use std::io::{Seek, SeekFrom};

use stable_eyre::{eyre::eyre, eyre::Context, Result};

use crate::{slice, BerkeleyDB};

use self::{entry::Entry, header::Header, index::Index, value::Value};

pub mod entry;
pub mod header;
pub mod index;
pub mod value;

pub fn read_entries(mut db: BerkeleyDB) -> Result<Vec<Value>> {
    let page_size = db
        .metadata
        .generic()
        .page_size()
        .try_into()
        .context("convert page size from u32")?;

    let mut values = Vec::new();
    for _ in 0..=db.metadata.generic().last_page_no() {
        let page = slice(&mut db.file, page_size).context("read page from file")?;
        let end_of_page_offset = db.file.stream_position().context("get current offset")?;

        let big_endian = db.metadata.big_endian();
        let header = Header::parse_dyn(page.as_slice(), big_endian).context("parse header")?;
        if header.should_skip() {
            continue;
        }

        let indexes = Index::parse_dyn(page.as_slice(), header.num_entries().into(), big_endian)
            .context("parse indexes")?;

        for index in indexes.into_iter() {
            // The first byte is the page type, so we can check it without parsing further.
            let page_type = page
                .get(usize::from(index))
                .ok_or_else(|| {
                    eyre!("block {index} did not exist in page, but was pointed at by index")
                })?
                .to_owned();

            // Skip pages that aren't the type we care about.
            if page_type != Value::SUPPORTED_PAGE_TYPE {
                continue;
            }

            let value = Value::parse_dyn(&mut db, page.as_slice(), index).context("parse value")?;

            values.push(value);
        }

        db.file
            .seek(SeekFrom::Start(end_of_page_offset))
            .context("seek to next page")?;
    }

    Ok(values)
}
