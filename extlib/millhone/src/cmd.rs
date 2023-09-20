use std::borrow::Cow;

use clap::Parser;
use getset::Getters;
use millhone::api;
use secrecy::Secret;
use tracing::warn;
use walkdir::DirEntry;

pub mod analyze;
pub mod ingest;
pub mod ping;

/// Arguments for API authentication.
#[derive(Debug, Parser, Getters)]
#[getset(get = "pub")]
pub struct ApiAuthentication {
    /// Provide the API Key ID for authentication.
    #[clap(long)]
    api_key_id: String,

    /// Provide the API Secret for authentication.
    #[clap(long)]
    api_secret: Secret<String>,
}

impl ApiAuthentication {
    /// Create credentials from these options.
    fn as_credentials(&self) -> api::Credentials {
        api::Credentials::new(self.api_key_id.clone(), self.api_secret.clone())
    }
}

/// Unwrap a directory entry, warning on error.
#[tracing::instrument]
fn unwrap_dir_entry(entry: Result<DirEntry, walkdir::Error>) -> Option<DirEntry> {
    match entry {
        Ok(entry) => Some(entry),
        Err(err) => {
            let depth = err.depth();
            let path = err
                .path()
                .map(|p| p.to_string_lossy())
                .unwrap_or_else(|| Cow::from("<none>"));
            if let Some(err) = err.io_error() {
                warn!(%path, %depth, %err, "walk: io error");
            } else if let Some(ancestor) = err.loop_ancestor() {
                let ancestor = ancestor.to_string_lossy();
                warn!(%path, %depth, %ancestor, "walk: symlink loop detected");
            } else {
                warn!(%path, %depth, "walk: generic error");
            }
            None
        }
    }
}
