use std::{
    collections::HashSet,
    io::{Read, Seek, SeekFrom},
};

use byteorder::{BigEndian, LittleEndian, ReadBytesExt};
use getset::CopyGetters;
use serde::Serialize;
use stable_eyre::{
    eyre::{bail, Context},
    Result,
};

use crate::parse::read_n;

/// https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/constants.go#L7
const HASH_MAGIC_NUMBER_BE: u32 = 0x61150600;

/// https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/constants.go#L6
const HASH_MAGIC_NUMBER: u32 = 0x00061561;

/// https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/constants.go#L18
const HASH_METADATA_PAGE_TYPE: u8 = 8;

/// https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/constants.go#L4
const NO_ENCRYPTION_ALGORITHM: u8 = 0;

/// https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/bdb.go#L11-L20
fn valid_page_sizes() -> HashSet<u32> {
    HashSet::from([512, 1024, 2048, 4096, 8192, 16384, 32768, 65536])
}

// Generic metadata for the entire DB.
//
// Reference: https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/generic_page.go#L11
#[derive(Debug, Default, PartialEq, Eq, CopyGetters, Serialize)]
#[get_copy = "pub"]
pub struct Generic {
    lsn: [u8; 8],             /* 00-07: LSN. */
    page_no: u32,             /* 08-11: Current page number. */
    magic: u32,               /* 12-15: Magic number. */
    version: u32,             /* 16-19: Version. */
    page_size: u32,           /* 20-23: Pagesize. */
    encryption_alg: u8,       /*    24: Encryption algorithm. */
    page_type: u8,            /*    25: Page type. */
    meta_flags: u8,           /*    26: Meta-only flags */
    _unused: u8,              /*    27: Unused. */
    free: u32,                /* 28-31: Free list page number. */
    last_page_no: u32,        /* 32-35: Page number of last page in db. */
    n_parts: u32,             /* 36-39: Number of partitions. */
    key_count: u32,           /* 40-43: Cached key count. */
    record_count: u32,        /* 44-47: Cached record count. */
    flags: u32,               /* 48-51: Flags: unique to each AM. */
    unique_file_id: [u8; 19], /* 52-71: Unique file ID. */
}

impl Generic {
    /// Read [`Self`] out of a file in plain byte order. Also returns the endianness of the data read using a check integer.
    ///
    /// Reference: https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/hash_metadata_page.go#L26-L27
    pub fn parse<F: Read + Seek>(r: &mut F) -> Result<(Self, bool)> {
        let generic = Self::parse_inner::<LittleEndian>(r).context("little endian")?;

        if generic.is_big_endian() {
            r.seek(SeekFrom::Start(0)).context("seek to start")?;
            let generic = Self::parse_inner::<BigEndian>(r).context("big endian")?;
            Ok((generic, true))
        } else {
            Ok((generic, false))
        }
    }

    pub fn parse_inner<E: byteorder::ByteOrder>(r: &mut impl Read) -> Result<Self> {
        Ok(Self {
            lsn: read_n(r)?,
            page_no: r.read_u32::<E>()?,
            magic: r.read_u32::<E>()?,
            version: r.read_u32::<E>()?,
            page_size: r.read_u32::<E>()?,
            encryption_alg: r.read_u8()?,
            page_type: r.read_u8()?,
            meta_flags: r.read_u8()?,
            _unused: r.read_u8()?,
            free: r.read_u32::<E>()?,
            last_page_no: r.read_u32::<E>()?,
            n_parts: r.read_u32::<E>()?,
            key_count: r.read_u32::<E>()?,
            record_count: r.read_u32::<E>()?,
            flags: r.read_u32::<E>()?,
            unique_file_id: read_n(r)?,
        })
    }

    /// Reference: https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/hash_metadata_page.go#L51,
    ///      plus: https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/bdb.go#L50-L52
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

    /// The `magic` field is a bit used to determine whether the struct was correctly parsed.
    /// If it is endian-flipped, it is used to indicate that the database should be parsed in the opposite endianness.
    ///
    /// Reference: https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/hash_metadata_page.go#L37-L44
    fn is_big_endian(&self) -> bool {
        self.magic == HASH_MAGIC_NUMBER_BE
    }
}
