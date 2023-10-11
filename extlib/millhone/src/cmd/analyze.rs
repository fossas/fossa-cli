use std::path::{Path, PathBuf};

use walkdir::WalkDir;
use clap::Parser;
use futures::{stream, StreamExt};
use getset::Getters;
use millhone::{api::prelude::*, extract::ContentSnippet};
use stable_eyre::{
    eyre::{bail, Context},
    Report,
};
use tap::Pipe;
use tracing::{debug, info, trace, warn};

use crate::cmd::{AtomicCounter, MatchingSnippet};

/// Options for snippet ingestion.
#[derive(Debug, Parser, Getters)]
#[getset(get = "pub")]
#[clap(version)]
pub struct Subcommand {
    /// The directory to which matches are output.
    #[clap(long, short)]
    output: PathBuf,

    /// If specified, overwrites the output directory if it exists.
    #[clap(long)]
    overwrite_output: bool,

    #[clap(flatten)]
    auth: super::ApiAuthentication,

    #[clap(flatten)]
    extract: millhone::extract::Options,
}

// #[tracing::instrument(skip_all, fields(target = %opts.extract.target().display()))]
pub async fn main(endpoint: &BaseUrl, opts: Subcommand) -> Result<(), Report> {
    debug!(?endpoint, ?opts, "analyzing local snippet matches");
    info!("Analyzing local snippet matches");

    if opts.output().exists() {
        if opts.overwrite_output {
            std::fs::remove_dir_all(opts.output()).context("remove existing output directory")?;
            debug!("removed existing output dir: {}", opts.output().display());
        } else {
            bail!(
                "the output directory '{}' already exists.",
                opts.output().display(),
            );
        }
    }
    if !opts.output().exists() {
        std::fs::create_dir_all(opts.output()).context("create output directory")?;
        debug!("created output dir: {}", opts.output().display());
    }

    let creds = opts.auth.as_credentials();
    let client = ApiClientV1::authenticated(endpoint, creds).context("create client")?;
    let root = opts.extract().target();
    let snippet_opts = opts.extract().into();

    let total_count_snippets = AtomicCounter::default();
    let total_count_matches = AtomicCounter::default();
    let total_count_files = AtomicCounter::default();

    WalkDir::new(root)
        // Follow symlinks; loops are yielded as errors automatically.
        .follow_links(true)
        // Not chosen for a specific reason, just seems reasonable.
        .max_depth(1000)
        // Just make the walk deterministic (per directory anyway).
        .sort_by_file_name()
        .into_iter()
        // Convert walk errors into warnings, then ignore that item.
        .filter_map(super::unwrap_dir_entry)
        // Turn this into a stream, which is an async iterator.
        .pipe(futures::stream::iter)
        // Resolve symlinks into full paths and filter to only files.
        .filter_map(|entry| {
            let total_count_files = &total_count_files;
            async move {
                debug!(path = %entry.path().display(), "resolve path");
                let path = super::resolve_path(&entry)
                    .await
                    .map_err(Report::from)
                    .map_err(|err| warn!(path = %entry.path().display(), "failed to resolve symlink to path: {err:#}"))
                    .ok()?;

                if path.is_file() {
                    debug!(path = %path.display(), "enqueued for processing");
                    total_count_files.increment();
                    Some(path)
                } else {
                    debug!(path = %path.display(), "skipped: not a file");
                    None
                }
            }
        })
        // Extract snippets from each file.
        .filter_map(|path| {
            let total_count_snippets = &total_count_snippets;
            async move {
                debug!(path = %path.display(), "extract snippets");
                let snippets = ContentSnippet::from_file(root, &snippet_opts, &path)
                    .await
                    .map_err(Report::from)
                    .map_err(|err| warn!(path = %path.display(), "extract snippets: {err:#}"))
                    .ok()?;

                if snippets.is_empty() {
                    debug!(path = %path.display(), "no snippets extracted");
                    return None;
                }

                let snippet_count = snippets.len();
                debug!(path = %path.display(), %snippet_count, "extracted snippets");
                total_count_snippets.increment_by(snippet_count);

                (path, snippets).pipe(Some)
            }
        })
        // Handle each file and its extracted snippets in parallel.
        .for_each_concurrent(None, |(path, snippets)| {
            let opts = &opts;
            let client = &client;
            let total_count_matches = &total_count_matches;
            async move {
                // The API accepts a single fingerprint at a time.
                let matches = stream::iter(snippets).filter_map(|found| {
                    let client = client.clone();
                    let total_count_matches = total_count_matches.clone();
                    let path = path.clone();
                    async move {
                        let fingerprint = found.snippet().fingerprint();
                        let matching_snippets = client
                            .lookup_snippets(fingerprint)
                            .await
                            .map_err(Report::from)
                            .map_err(|err| warn!(path = %path.display(), %fingerprint, "lookup snippet: {err:#}"))
                            .ok()?;
                        if matching_snippets.is_empty() {
                            trace!(%fingerprint, "no matches in corpus");
                            return None;
                        }
                        for matched in matching_snippets.iter() {
                            trace!(%fingerprint, ?matched, "matched snippet");
                        }
                        total_count_matches.increment_by(matching_snippets.len());
        
                        MatchingSnippet::builder()
                            .found_in(found.snippet().file_path())
                            .local_snippet(found.snippet().clone())
                            .local_text(String::from_utf8_lossy(found.content()))
                            .matching_snippets(matching_snippets)
                            .build()
                            .pipe(Some)
                    }
                })
                .collect::<Vec<_>>()
                .await;

                match write_match_records(root, opts, &path, &matches).await {
                    Ok(record_path) => debug!(
                        match_count = %matches.len(),
                        file = %path.display(),
                        record = %record_path.display(),
                        "wrote matches",
                    ),
                    Err(err) => warn!(file = %path.display(), "failed to write match records: {err:#}"),
                }
            }
        })
        .await;

    info!(
        "Finished matching {} snippets out of {} files to {} matches",
        total_count_snippets.snapshot(),
        total_count_files.snapshot(),
        total_count_matches.snapshot(),
    );

    Ok(())
}

#[tracing::instrument(skip_all, fields(for_file = %path.display(), match_count = %records.len()))]
async fn write_match_records(
    root: &Path,
    opts: &Subcommand,
    path: &Path,
    records: &[MatchingSnippet],
) -> Result<PathBuf, Report> {
    let record_name = path
        .strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace(std::path::MAIN_SEPARATOR_STR, "_");

    let current_ext = path.extension().and_then(|ext| ext.to_str()).unwrap_or("");
    let record_path = opts
        .output()
        .join(record_name)
        .with_extension(format!("{current_ext}.json"));

    let rendered = serde_json::to_string_pretty(&records).context("encode match records")?;
    tokio::fs::write(&record_path, rendered)
        .await
        .wrap_err_with(|| format!("write match records to '{}'", record_path.display(),))?;

    Ok(record_path)
}
