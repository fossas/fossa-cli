use std::fs;

use clap::Parser;
use getset::Getters;
use millhone::{
    api::{prelude::*, Credentials},
    extract::Snippet,
};
use secrecy::Secret;
use stable_eyre::eyre::Context;
use tracing::{debug, info, warn};
use walkdir::WalkDir;

/// Options for snippet ingestion.
#[derive(Debug, Parser, Getters)]
#[getset(get = "pub")]
#[clap(version)]
pub struct Options {
    /// Provide the API Key ID for authentication.
    #[clap(long)]
    api_key_id: String,

    /// Provide the API Secret for authentication.
    #[clap(long)]
    api_secret: Secret<String>,

    #[clap(flatten)]
    extract: millhone::extract::Options,
}

#[tracing::instrument(skip_all, fields(target = %opts.extract.target().display()))]
pub fn main(endpoint: &BaseUrl, opts: Options) -> stable_eyre::Result<()> {
    info!(
        api_key_id = %opts.api_key_id(),
        "Analyzing local snippet matches",
    );

    let creds = Credentials::new(opts.api_key_id().clone(), opts.api_secret().clone());
    let client = ApiClientV1::authenticated(endpoint, creds);
    let walk = WalkDir::new(opts.extract().target())
        // Follow symlinks; loops are yielded as errors automatically.
        .follow_links(true)
        // Not chosen for a specific reason, just seems reasonable.
        .max_depth(1000)
        // Just make the walk deterministic (per directory anyway).
        .sort_by_file_name();

    let mut total_count_entries = 0usize;
    let mut total_count_snippets = 0usize;
    let mut total_count_files = 0usize;

    // Future enhancement: walk and analyze in parallel with rayon.
    let snippet_opts = opts.extract().into();
    for entry in walk.into_iter() {
        total_count_entries += 1;
        let Some(entry) = super::unwrap_dir_entry(entry) else {
            continue;
        };

        let path = if entry.path_is_symlink() {
            let path = entry.path();
            fs::read_link(path)
                .wrap_err_with(|| format!("resolve symlink of '{}'", path.display()))?
        } else {
            entry.path().to_path_buf()
        };

        if !path.is_file() {
            debug!(path = %path.display(), "skipped: not a file");
            continue;
        }

        total_count_files += 1;
        info!(path = %path.display(), "analyze");

        let snippets = Snippet::from_file(&snippet_opts, &path)
            .wrap_err_with(|| format!("process '{}'", path.display()))?;

        info!(snippet_count = %snippets.len(), "match snippets");
        total_count_snippets += snippets.len();
    }

    info!(
        %total_count_entries,
        %total_count_snippets,
        %total_count_files,
        "Finished extracting snippets",
    );
    Ok(())
}
