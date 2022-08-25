use getset::CopyGetters;
use stable_eyre::Result;
use std::io::Read;

use byteorder::{ByteOrder, ReadBytesExt};

#[derive(Debug, Default, PartialEq, Eq, CopyGetters)]
#[get_copy = "pub"]
pub struct Hash {
    max_bucket: u32,
    high_mask: u32,
    low_mask: u32,
    fill_factor: u32,
    num_keys: u32,
    char_key_hash: u32,
}

impl Hash {
    /// Read [`Self`] out of a file in plain byte order.
    pub fn parse<E: ByteOrder>(r: &mut impl Read) -> Result<Self> {
        Ok(Self {
            max_bucket: r.read_u32::<E>()?,
            high_mask: r.read_u32::<E>()?,
            low_mask: r.read_u32::<E>()?,
            fill_factor: r.read_u32::<E>()?,
            num_keys: r.read_u32::<E>()?,
            char_key_hash: r.read_u32::<E>()?,
        })
    }

    pub fn validate(&self) -> Result<()> {
        Ok(())
    }
}
