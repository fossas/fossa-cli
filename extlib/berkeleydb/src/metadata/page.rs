use getset::{CopyGetters, Getters};
use stable_eyre::{eyre::Context, Result};
use typed_builder::TypedBuilder;

use super::{Generic, Hash};

/// A collection of metadata for the database.
///
/// Reference: https://github.com/jssblck/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/hash_metadata_page.go#L22
#[derive(Debug, Default, PartialEq, Eq, Getters, CopyGetters, TypedBuilder)]
pub struct Page {
    #[get = "pub"]
    generic: Generic,

    #[get = "pub"]
    hash: Hash,

    #[get_copy = "pub"]
    big_endian: bool, // Renamed from `Swapped` in the Go implementation.
}

impl Page {
    pub fn validate(&self) -> Result<()> {
        self.generic.validate().context("validate generic")?;
        self.hash.validate().context("validate hash")?;
        Ok(())
    }
}
