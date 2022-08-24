use stable_eyre::{eyre::Context, Result};

use super::{Generic, Hash};

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Page {
    pub generic: Generic,
    pub hash: Hash,
    pub big_endian: bool,
}

impl Page {
    pub fn validate(&self) -> Result<()> {
        self.generic.validate().context("validate generic")?;
        self.hash.validate().context("validate hash")?;
        Ok(())
    }
}
