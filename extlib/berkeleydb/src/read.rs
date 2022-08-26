use std::io::{Seek, SeekFrom};

use log::debug;
use stable_eyre::{
    eyre::{bail, Context},
    Result,
};

use crate::{parse::slice, BerkeleyDB};

use self::{entry::Entry, header::Header, index::Index, value::Value};

pub mod entry;
pub mod header;
pub mod index;
pub mod value;

impl BerkeleyDB {
    pub fn read(&mut self) -> Result<Vec<Value>> {
        debug!("🔬 Reading values from DB");

        let page_size = self
            .metadata
            .generic()
            .page_size()
            .try_into()
            .context("convert page size from u32")?;

        let mut values = Vec::new();
        for page_num in 0..=self.metadata.generic().last_page_no() {
            debug!("🔢 Reading page {page_num}");

            let page = slice(&mut self.file, page_size).context("read page from file")?;
            let end_of_page_offset = self.file.stream_position().context("get current offset")?;

            let big_endian = self.metadata.big_endian();
            let header = Header::parse_dyn(page.as_slice(), big_endian).context("parse header")?;
            if header.should_skip() {
                debug!("🔁 Skip page {page_num}; header: {header:?}");
                continue;
            }

            let indexes =
                Index::parse_dyn(page.as_slice(), header.num_entries().into(), big_endian)
                    .context("parse indexes")?;

            debug!("📇 Found index pointers: {}", indexes.len());
            for (index_num, index) in indexes.into_iter().enumerate() {
                debug!("🔢 Reading pointer {index_num}");

                // The first byte is the page type, so we can check it without parsing further.
                let page_type = if let Some(page_type) = page.get(usize::from(index)) {
                    page_type.to_owned()
                } else {
                    bail!("block {index} did not exist in page, but was pointed at by index")
                };

                // Skip pages that aren't the type we care about.
                if page_type != Value::SUPPORTED_PAGE_TYPE {
                    debug!("🔁 Skip pointer {index_num}; page type not supported: {page_type}");
                    continue;
                }

                let value =
                    Value::parse_dyn(self, page.as_slice(), index).context("parse value")?;

                values.push(value);
            }

            debug!("✅ Done building value from indexes, moving to next page");
            self.file
                .seek(SeekFrom::Start(end_of_page_offset))
                .context("seek to next page")?;
        }

        debug!("✅ Done reading values; read: {}", values.len());
        Ok(values)
    }
}
