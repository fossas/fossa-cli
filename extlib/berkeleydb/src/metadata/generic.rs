use std::io::Read;

use byteorder::ReadBytesExt;
use stable_eyre::{eyre::bail, Result};

use super::{
    shared::{
        read_n, valid_page_sizes, HASH_MAGIC_NUMBER, HASH_METADATA_PAGE_TYPE,
        NO_ENCRYPTION_ALGORITHM,
    },
    HASH_MAGIC_NUMBER_BE,
};

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Generic {
    lsn: [u8; 8],
    page_no: u32,
    magic: u32,
    version: u32,
    page_size: u32,
    encryption_alg: u8,
    page_type: u8,
    meta_flags: u8,
    unused1: u8,
    free: u32,
    last_page_no: u32,
    n_parts: u32,
    key_count: u32,
    record_count: u32,
    flags: u32,
    unique_file_id: [u8; 19],
}

impl Generic {
    pub fn parse<E: byteorder::ByteOrder>(r: &mut impl Read) -> Result<Self> {
        Ok(Self {
            lsn: read_n(r)?,
            page_no: r.read_u32::<E>()?,
            magic: r.read_u32::<E>()?,
            version: r.read_u32::<E>()?,
            page_size: r.read_u32::<E>()?,
            encryption_alg: r.read_u8()?,
            page_type: r.read_u8()?,
            meta_flags: r.read_u8()?,
            unused1: r.read_u8()?,
            free: r.read_u32::<E>()?,
            last_page_no: r.read_u32::<E>()?,
            n_parts: r.read_u32::<E>()?,
            key_count: r.read_u32::<E>()?,
            record_count: r.read_u32::<E>()?,
            flags: r.read_u32::<E>()?,
            unique_file_id: read_n(r)?,
        })
    }

    pub fn validate(&self) -> Result<()> {
        if self.magic != HASH_MAGIC_NUMBER {
            bail!("unexpected DB magic number: {}", self.magic);
        }

        if self.page_type != HASH_METADATA_PAGE_TYPE {
            bail!("unexpected page type: {}", self.page_type);
        }

        if self.encryption_alg != NO_ENCRYPTION_ALGORITHM {
            bail!("unexpected encryption algorithm: {}", self.encryption_alg);
        }

        if !valid_page_sizes().contains(&self.page_size) {
            bail!("unexpected page size: {}", self.page_size);
        }

        Ok(())
    }

    pub fn is_big_endian(&self) -> bool {
        self.magic == HASH_MAGIC_NUMBER_BE
    }
}
