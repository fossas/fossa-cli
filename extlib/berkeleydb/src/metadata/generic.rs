use std::{collections::HashSet, io::Read};

use byteorder::ReadBytesExt;
use getset::CopyGetters;
use stable_eyre::{eyre::bail, Result};

use crate::read_n;

const HASH_MAGIC_NUMBER_BE: u32 = 0x61150600;
const HASH_MAGIC_NUMBER: u32 = 0x00061561;

const HASH_METADATA_PAGE_TYPE: u8 = 8;

const NO_ENCRYPTION_ALGORITHM: u8 = 0;

fn valid_page_sizes() -> HashSet<u32> {
    HashSet::from([512, 1024, 2048, 4096, 8192, 16384, 32768, 65536])
}

#[derive(Debug, Default, PartialEq, Eq, CopyGetters)]
#[get_copy = "pub"]
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
    /// Read [`Self`] out of a file in plain byte order.
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
