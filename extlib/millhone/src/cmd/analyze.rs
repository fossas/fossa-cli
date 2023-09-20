use std::fs;

use clap::Parser;
use getset::Getters;
use millhone::{api::prelude::*, extract::Snippet};
use stable_eyre::{eyre::Context, Report};
use tracing::{debug, info, warn};
use walkdir::WalkDir;

/// Options for snippet ingestion.
#[derive(Debug, Parser, Getters)]
#[getset(get = "pub")]
#[clap(version)]
pub struct Subcommand {
    #[clap(flatten)]
    auth: super::ApiAuthentication,

    #[clap(flatten)]
    extract: millhone::extract::Options,
}

#[tracing::instrument(skip_all, fields(target = %opts.extract.target().display()))]
pub fn main(endpoint: &BaseUrl, opts: Subcommand) -> Result<(), Report> {
    info!(
        api_key_id = %opts.auth.api_key_id(),
        "Analyzing local snippet matches",
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

        let snippets = Snippet::from_file(root, &snippet_opts, &path)
            .wrap_err_with(|| format!("process '{}'", path.display()))?;

        info!(snippet_count = %snippets.len(), "extracted snippets");
        total_count_snippets += snippets.len();

        for snippet in snippets {
            let fingerprint = snippet.fingerprint();
            let matching_snippets = client
                .lookup_snippets(fingerprint)
                .wrap_err_with(|| format!("lookup snippet for fingerprint '{fingerprint}'"))?;

            // As of now just log matching snippets; functionality to write to disk is coming in another PR.
            for matching_snippet in matching_snippets {
                info!(%fingerprint, "matching snippet: {matching_snippet:#?}");
            }
        }
    }

    info!(
        %total_count_entries,
        %total_count_snippets,
        %total_count_files,
        "Finished extracting snippets",
    );
    Ok(())
}
