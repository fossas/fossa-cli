use getset::CopyGetters;
use stable_eyre::Result;
use std::io::Read;

use byteorder::{ByteOrder, ReadBytesExt};

use crate::parse::ByteParser;

/// Hash metadata.
///
/// This is intended to be parsed immediately _after_ parsing [`crate::metadata::Generic`];
/// as such the byte offsets reflect the offset after doing so.
///
/// There are more fields in the actual data structure than what we parse here,
/// but they are ignored in the Go implementation: https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/hash_metadata_page.go#L19
/// Given this we ignore them here too.
///
/// Reference: https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/hash_metadata_page.go#L11
#[derive(Debug, Default, PartialEq, Eq, CopyGetters)]
#[get_copy = "pub"]
pub struct Hash {
    max_bucket: u32,    /* 72-75: ID of Maximum bucket in use */
    high_mask: u32,     /* 76-79: Modulo mask into table */
    low_mask: u32,      /* 80-83: Modulo mask into table lower half */
    fill_factor: u32,   /* 84-87: Fill factor */
    num_keys: u32,      /* 88-91: Number of keys in hash table */
    char_key_hash: u32, /* 92-95: Value of hash(CHARKEY) */
}

impl Hash {
    /// This validation exists for completeness, but is currently a no-op.
    pub fn validate(&self) -> Result<()> {
        Ok(())
    }
}

impl ByteParser for Hash {
    type Output = Self;

    /// Read [`Self`] out of a file in plain byte order.
    fn parse<E: ByteOrder>(r: &mut impl Read) -> Result<Self::Output> {
        Ok(Self {
            max_bucket: r.read_u32::<E>()?,
            high_mask: r.read_u32::<E>()?,
            low_mask: r.read_u32::<E>()?,
            fill_factor: r.read_u32::<E>()?,
            num_keys: r.read_u32::<E>()?,
            char_key_hash: r.read_u32::<E>()?,
        })
    }
}
