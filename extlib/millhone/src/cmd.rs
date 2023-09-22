use std::{borrow::Cow, path::PathBuf};

use clap::Parser;
use getset::Getters;
use itertools::Itertools;
use millhone::api;
use once_cell::sync::Lazy;
use secrecy::Secret;
use tap::Pipe;
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

static TEMP_PREFIX: Lazy<String> = Lazy::new(|| {
    let name = env!("CARGO_PKG_NAME");
    format!("{name}_")
});

/// Create a temporary directory prefixed by the name of the current crate in the system temp directory.
pub fn namespaced_temp_dir() -> Result<PathBuf, std::io::Error> {
    tempfile::Builder::new()
        .prefix(TEMP_PREFIX.as_str())
        .tempdir()
        .map(|tf| tf.into_path())
}

/// Find the latest temporary directory created by [`namespaced_temp_dir`].
///
/// Note: this is allowed as dead code for now but code that uses it (via the `commit` subcommand)
/// is coming in another PR.
#[allow(dead_code)]
pub fn latest_temp_dir() -> Result<Option<PathBuf>, std::io::Error> {
    let temp = std::env::temp_dir();
    let mut files = std::fs::read_dir(temp)?
        .filter_ok(|entry| {
            entry
                .file_name()
                .to_str()
                .map(|name| name.starts_with(TEMP_PREFIX.as_str()))
                .unwrap_or_default()
        })
        .collect::<Result<Vec<_>, _>>()?;

    // `sort_by_key` operates in ascending order; we want the first in descending order
    // (which is equivalent to "the one created most recently").
    files.sort_by_key(|entry| entry.metadata().and_then(|m| m.created()).ok());
    files.last().map(|entry| entry.path()).pipe(Ok)
}

#[cfg(test)]
mod tests {

    use std::{path::Path, thread::sleep, time::Duration};

    use super::*;

    use tap::TapFallible;

    fn cleanup(dir: &Path) {
        if let Err(err) = std::fs::remove_dir_all(dir) {
            eprintln!(
                "[warn] failed to clean up directory '{}': {err:#}",
                dir.display()
            );
        }
    }

    #[test]
    fn finds_latest_temp_dir() {
        let a = namespaced_temp_dir().expect("create 'a'");

        // Windows, unfortunately, was flaky without this.
        sleep(Duration::from_secs(1));

        let b = namespaced_temp_dir()
            .tap_err(|_| cleanup(&a))
            .expect("create 'b'");
        let latest = latest_temp_dir()
            .tap_err(|_| cleanup(&a))
            .tap_err(|_| cleanup(&b))
            .expect("find latest");

        assert_eq!(latest, Some(b));
    }
}
