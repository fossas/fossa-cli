use std::{
    collections::VecDeque,
    io::{Seek, SeekFrom},
};

use stable_eyre::{eyre::eyre, eyre::Context, Result};

use crate::{read_n_dyn, BerkeleyDB};

use self::{entry::Entry, header::Header, index::Index, value::Value};

pub mod entry;
pub mod header;
pub mod index;
pub mod value;

pub(crate) struct EntryReader {
    db: BerkeleyDB,

    /// We read multiple entries at once from a single page, then buffer them for the iterator to work.
    entries: VecDeque<Value>,

    /// Page size is stored in metadata as a `u32`. We convert it at construction time to `usize` and store it.
    page_size: usize,

    /// Tracks how far the iterator has read in the DB.
    page_num: u32,
}

impl EntryReader {
    pub(crate) fn new(db: BerkeleyDB) -> Result<Self> {
        let page_size = db
            .metadata
            .generic()
            .page_size()
            .try_into()
            .context("convert page size from u32")?;

        Ok(Self {
            db,
            page_size,
            page_num: 0,
            entries: VecDeque::new(),
        })
    }
}

impl Iterator for EntryReader {
    type Item = Result<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        let last_page_no = self.db.metadata.generic().last_page_no();
        if self.page_num > last_page_no {
            return None;
        }

        // `read_page` is split into a different function to make its error handling simpler,
        // but logically it's really part of this function.
        while self.page_num <= last_page_no {
            // `read_page` signals that a page should be skipped by returning an empty vec.
            let entries = read_page(self);
            self.page_num += 1;
            match entries {
                Ok(values) => {
                    if values.is_empty() {
                        continue;
                    } else {
                        self.entries.extend(values.into_iter());
                        return Ok(self.entries.pop_front()).transpose();
                    }
                }
                Err(err) => return Some(Err(err)),
            }
        }

        // If the loop is read all the way through and all pages were skipped (or there were no pages)
        // then end the iterator.
        None
    }
}

fn read_page(r: &mut EntryReader) -> Result<Vec<Value>> {
    let page = read_n_dyn(&mut r.db.file, r.page_size).context("read page from file")?;
    let page = page.as_slice();
    let end_of_page_offset = r.db.file.stream_position().context("get current offset")?;

    let big_endian = r.db.metadata.big_endian();
    let header = Header::parse_dyn(page, big_endian).context("parse header")?;
    if header.should_skip() {
        return Ok(vec![]);
    }

    let indexes = Index::parse_dyn(
        page,
        header.num_entries().into(),
        r.db.metadata.big_endian(),
    )
    .context("parse indexes")?;

    let mut values = Vec::new();
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

        let value = Value::parse_dyn(
            &mut r.db.file,
            page,
            index,
            r.db.metadata.generic().page_size(),
            big_endian,
        )
        .context("parse value")?;
        values.push(value);
    }

    r.db.file
        .seek(SeekFrom::Start(end_of_page_offset))
        .context("seek to next page")?;

    Ok(values)
}
