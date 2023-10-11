use std::collections::HashSet;

use clap::Parser;
use futures::executor::block_on;
use getset::Getters;
use millhone::{
    api::{prelude::*, ApiSnippet},
    extract::Snippet,
};
use rayon::prelude::*;
use srclib::Locator;
use stable_eyre::{eyre::Context, Report};
use tap::Pipe;
use tracing::{debug, info, warn};
use uuid::Uuid;
use walkdir::WalkDir;

use crate::cmd::AtomicCounter;

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
pub fn main(endpoint: &BaseUrl, opts: Subcommand) -> Result<(), Report> {
    debug!(?opts, "ingesting snippets");
    info!("Ingesting snippets");

    let creds = opts.auth.as_credentials();
    let client = ApiClientV1::authenticated(endpoint, creds).context("create client")?;
    let root = opts.extract().target();
    let snippet_opts = opts.extract().into();

    let total_count_entries = AtomicCounter::default();
    let total_count_snippets = AtomicCounter::default();
    let total_count_files = AtomicCounter::default();

    WalkDir::new(root)
        // Follow symlinks; loops are yielded as errors automatically.
        .follow_links(true)
        // Not chosen for a specific reason, just seems reasonable.
        .max_depth(1000)
        // Just make the walk deterministic (per directory anyway).
        .sort_by_file_name()
        .into_iter()
        .inspect(|_| total_count_entries.increment())
        .filter_map(super::unwrap_dir_entry)
        // Bridge into rayon for parallelization.
        .par_bridge()
        // Execute each entry in parallel.
        .try_for_each(|entry| -> Result<(), Report> {
            let path = super::resolve_path(&entry)
                .pipe(block_on)
                .context("resolve path for entry")?;
            if !path.is_file() {
                debug!(path = %path.display(), "skipped: not a file");
                return Ok(());
            }

            total_count_files.increment();
            debug!(path = %path.display(), "extract snippets");
            let snippets = Snippet::from_file(root, &snippet_opts, &path)
                .wrap_err_with(|| format!("extract snippets from '{}'", path.display()))?
                .into_iter()
                .map(|snippet| ApiSnippet::from(opts.ingest_id(), opts.locator(), snippet))
                .collect::<HashSet<_>>();

            if snippets.is_empty() {
                debug!(path = %path.display(), "no snippets extracted");
                return Ok(());
            }

            let snippet_count = snippets.len();
            client.add_snippets(snippets).wrap_err_with(|| {
                format!("upload {snippet_count} snippets from '{}'", path.display())
            })?;

            total_count_snippets.increment_by(snippet_count);
            debug!(path = %path.display(), %snippet_count, "extracted snippets");
            Ok(())
        })?;

    info!(
        "Finished extracting {} snippets out of {} entries, of which {} were files",
        total_count_snippets.into_inner(),
        total_count_entries.into_inner(),
        total_count_files.into_inner(),
    );
    Ok(())
}
