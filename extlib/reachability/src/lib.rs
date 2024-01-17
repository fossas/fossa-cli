//! Parses language source code to create call graphs.
//!
//! # Supported Parser
//! - Java
use log::debug;
use serde::Serialize;
use snippets::content::Content;
use stable_eyre::{eyre::Context, Result};
use std::io::{self, Read};
use std::path::PathBuf;

#[derive(Debug, Serialize)]
pub struct Reachability {}

impl Reachability {
    pub fn from_stdin() -> Result<Self> {
        let mut stdin = String::new();
        io::stdin()
            .read_to_string(&mut stdin)
            .context("buffer stdin")?;
        Reachability::from_string(&stdin)
    }

    pub fn from_string(s: &str) -> Result<Self> {
        let _content = Content::new(s.into());
        todo!();
    }

    pub fn from_fs(path: &PathBuf) -> Result<Self> {
        debug!("Parsing file system: {path:?}");
        todo!();
    }
}
