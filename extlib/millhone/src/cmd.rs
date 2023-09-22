use std::{borrow::Cow, collections::HashSet};

use clap::Parser;
use getset::Getters;
use millhone::{
    api::{self, ApiSnippet},
    extract::Snippet,
};
use secrecy::Secret;
use serde::{Deserialize, Serialize};
use tracing::warn;
use typed_builder::TypedBuilder;
use walkdir::DirEntry;

pub mod analyze;
pub mod commit;
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

/// A snippet match found in a local file during analysis.
#[derive(Debug, Clone, Serialize, Deserialize, TypedBuilder)]
pub struct MatchingSnippet {
    /// The local path in which the match was found, rendered as a string.
    /// Any invalid UTF8 content is replaced by `U+FFFD`.
    #[builder(setter(into))]
    found_in: String,

    /// A copy of the file content indicated by `found_at`, rendered as a string.
    /// Any invalid UTF8 content is replaced by `U+FFFD`.
    #[builder(setter(into))]
    local_text: String,

    /// The snippet that was identified in the local project.
    local_snippet: Snippet,

    /// Snippets in the knowledgebase that match this snippet.
    #[builder(setter(into))]
    matching_snippets: HashSet<ApiSnippet>,
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
