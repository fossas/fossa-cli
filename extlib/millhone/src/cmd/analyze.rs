use std::{collections::HashSet, fs, path::PathBuf};

use clap::Parser;
use getset::Getters;
use millhone::{
    api::{prelude::*, ApiSnippet},
    extract::{ContentSnippet, Snippet},
};
use serde::{Deserialize, Serialize};
use stable_eyre::{eyre::Context, Report};
use tracing::{debug, info, warn};
use typed_builder::TypedBuilder;
use walkdir::WalkDir;

/// Options for snippet ingestion.
#[derive(Debug, Parser, Getters)]
#[getset(get = "pub")]
#[clap(version)]
pub struct Subcommand {
    /// The directory to which matches are output.
    ///
    /// If not specified, matches are output to a temporary directory, which is not cleaned up on exit.
    /// The path to this temporary directory is written to `stdout` follwed by a newline.
    /// It is the responsibility of the user to clean up this directory after analysis.
    #[clap(long, short)]
    output: Option<PathBuf>,

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

    let output_dir = match opts.output() {
        Some(dir) => dir.to_owned(),
        None => super::namespaced_temp_dir()?,
    };

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

        let snippets = ContentSnippet::from_file(root, &snippet_opts, &path)
            .wrap_err_with(|| format!("process '{}'", path.display()))?;

        info!(snippet_count = %snippets.len(), "extracted snippets");
        total_count_snippets += snippets.len();
        if snippets.is_empty() {
            continue;
        }

        let mut records = Vec::new();
        for found in snippets {
            let fingerprint = found.snippet().fingerprint();
            let matching_snippets = client
                .lookup_snippets(fingerprint)
                .wrap_err_with(|| format!("lookup snippet for fingerprint '{fingerprint}'"))?;

            let record = MatchingSnippet::builder()
                .found_in(found.snippet().file_path())
                .local_snippet(found.snippet().clone())
                .local_text(String::from_utf8_lossy(found.content()))
                .matching_snippets(matching_snippets)
                .build();

            records.push(record);
        }

        let record_name = path
            .strip_prefix(root)
            .unwrap_or(&path)
            .to_string_lossy()
            .replace(std::path::MAIN_SEPARATOR_STR, "_");

        let record_path = output_dir.join(&record_name);
        let encoded = serde_json::to_string_pretty(&records).context("encode record")?;
        std::fs::write(&record_path, encoded).context("write records")?;

        info!(
            "wrote {} match record(s) for '{}' to '{}'",
            records.len(),
            record_name,
            record_path.display()
        );
    }

    info!(
        %total_count_entries,
        %total_count_snippets,
        %total_count_files,
        "Finished extracting snippets",
    );

    println!("{}", output_dir.display());
    Ok(())
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
