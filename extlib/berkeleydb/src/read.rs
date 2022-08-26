use std::io::{Seek, SeekFrom};

use stable_eyre::{eyre::eyre, eyre::Context, Result};

use crate::{parse::slice, BerkeleyDB};

use self::{entry::Entry, header::Header, index::Index, value::Value};

pub mod entry;
pub mod header;
pub mod index;
pub mod value;

impl BerkeleyDB {
    pub fn read(&mut self) -> Result<Vec<Value>> {
        let page_size = self
            .metadata
            .generic()
            .page_size()
            .try_into()
            .context("convert page size from u32")?;

        let mut values = Vec::new();
        for _ in 0..=self.metadata.generic().last_page_no() {
            let page = slice(&mut self.file, page_size).context("read page from file")?;
            let end_of_page_offset = self.file.stream_position().context("get current offset")?;

            let big_endian = self.metadata.big_endian();
            let header = Header::parse_dyn(page.as_slice(), big_endian).context("parse header")?;
            if header.should_skip() {
                continue;
            }

            let indexes =
                Index::parse_dyn(page.as_slice(), header.num_entries().into(), big_endian)
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

                let value =
                    Value::parse_dyn(self, page.as_slice(), index).context("parse value")?;

                values.push(value);
            }

            self.file
                .seek(SeekFrom::Start(end_of_page_offset))
                .context("seek to next page")?;
        }

        Ok(values)
    }
}
