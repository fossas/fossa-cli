use getset::{CopyGetters, Getters};
use stable_eyre::{eyre::Context, Result};
use typed_builder::TypedBuilder;

use super::{Generic, Hash};

#[derive(Debug, Default, PartialEq, Eq, Getters, CopyGetters, TypedBuilder)]
pub struct Page {
    #[get = "pub"]
    generic: Generic,

    #[get = "pub"]
    hash: Hash,

    #[get_copy = "pub"]
    big_endian: bool,
}

impl Page {
    pub fn validate(&self) -> Result<()> {
        self.generic.validate().context("validate generic")?;
        self.hash.validate().context("validate hash")?;
        Ok(())
    }
}
