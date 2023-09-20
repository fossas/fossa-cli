use std::{collections::HashSet, fs};

use clap::Parser;
use getset::Getters;
use millhone::{
    api::{prelude::*, ApiSnippet},
    extract::Snippet,
};
use srclib::Locator;
use stable_eyre::eyre::Context;
use tracing::{debug, info, warn};
use uuid::Uuid;
use walkdir::WalkDir;

/// Options for snippet ingestion.
#[derive(Debug, Parser, Getters)]
#[getset(get = "pub")]
#[clap(version)]
pub struct Subcommand {
    /// Provide the locator to which this snippet should be ingested.
    /// Note that this must be a full locator (including revision).
    #[clap(long, value_parser = Locator::parse)]
    locator: Locator,

    /// Provide the ingest ID for the ingestion.
    /// If not provided, defaults to a new UUID.
    #[clap(long, default_value_t = Uuid::new_v4().to_string())]
    ingest_id: String,

    #[clap(flatten)]
    extract: millhone::extract::Options,

    #[clap(flatten)]
    auth: super::ApiAuthentication,
}

#[tracing::instrument(skip_all, fields(target = %opts.extract().target().display()))]
pub fn main(endpoint: &BaseUrl, opts: Subcommand) -> stable_eyre::Result<()> {
    info!(
        ingest_id = %opts.ingest_id(),
        api_key_id = %opts.auth.api_key_id(),
        locator = %opts.locator(),
        "Ingesting snippets",
    );

    let creds = opts.auth.as_credentials();
    let client = ApiClientV1::authenticated(endpoint, creds);
    let root = opts.extract().target();
    let walk = WalkDir::new(root)
        // Follow symlinks; loops are yielded as errors automatically.
        .follow_links(true)
        // Not chosen for a specific reason, just seems reasonable.
        .max_depth(1000)
        // Just make the walk deterministic (per directory anyway).
        .sort_by_file_name();

    let mut total_count_entries = 0usize;
    let mut total_count_snippets = 0usize;
    let mut total_count_files = 0usize;

    // Future enhancement: walk and upload in parallel with rayon.
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
        info!(path = %path.display(), "ingest");
        let snippets = Snippet::from_file(root, &snippet_opts, &path)
            .wrap_err_with(|| format!("process '{}'", path.display()))?
            .into_iter()
            .map(|snippet| ApiSnippet::from(opts.ingest_id(), opts.locator(), snippet))
            .collect::<HashSet<_>>();

        info!(snippet_count = %snippets.len(), "upload snippets");
        total_count_snippets += snippets.len();
        client
            .add_snippets(snippets)
            .wrap_err_with(|| format!("upload snippets from '{}'", path.display()))?;
    }

    info!(
        %total_count_entries,
        %total_count_snippets,
        %total_count_files,
        "Finished extracting snippets",
    );
    Ok(())
}
