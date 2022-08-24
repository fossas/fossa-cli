use std::{collections::HashSet, io::Read};

use stable_eyre::Result;

pub const HASH_MAGIC_NUMBER_BE: u32 = 0x61150600;
pub const HASH_MAGIC_NUMBER: u32 = 0x00061561;

pub const HASH_UNSORTED_PAGE_TYPE: u8 = 2;
pub const OVERFLOW_PAGE_TYPE: u8 = 7;
pub const HASH_METADATA_PAGE_TYPE: u8 = 8;
pub const HASH_PAGE_TYPE: u8 = 13;

pub const NO_ENCRYPTION_ALGORITHM: u8 = 0;

pub fn valid_page_sizes() -> HashSet<u32> {
    HashSet::from([512, 1024, 2048, 4096, 8192, 16384, 32768, 65536])
}

pub fn read_n<const N: usize>(r: &mut impl Read) -> Result<[u8; N]> {
    let mut buf = [0; N];
    r.read_exact(&mut buf)?;
    Ok(buf)
}
