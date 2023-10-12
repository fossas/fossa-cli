use std::{
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

use clap::Parser;
use futures::{stream, StreamExt};
use getset::Getters;
use millhone::{api::prelude::*, extract::ContentSnippet};
use stable_eyre::{
    eyre::{bail, Context},
    Report,
};
use tap::Pipe;
use tokio::sync::Semaphore;
use tracing::{debug, info, trace, warn};
use walkdir::WalkDir;

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

#[tracing::instrument(skip_all, fields(target = %opts.extract.target().display()))]
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

    let concurrency_limiter = opts.extract.concurrency().pipe(Semaphore::new);

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
            // These let bindings are awkward,
            // but needed since we're using `move` for the async block.
            //
            // In the future we should probably clean this up a bit
            // (extract some of these closures into functions and/or
            // use macros to make this a little more ergonomic).
            let total_count_files = &total_count_files;
            let concurrency_limiter = &concurrency_limiter;
            async move {
                let _permit = concurrency_limiter.acquire().await.expect("concurrency limiter has been closed");
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
            let concurrency_limiter = &concurrency_limiter;
            async move {
                let _permit = concurrency_limiter.acquire().await.expect("concurrency limiter has been closed");
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
            let concurrency_limiter = &concurrency_limiter;
            async move {
                // The API accepts a single fingerprint at a time.
                // Do the lookups concurrently.
                // 
                // We use an `Arc<Mutex<Vec<_>>>` here because it's simpler to do it this way
                // than to build it over an iterator, since we want to do the lookups in parallel.
                //
                // Note: This uses a synchronous `Mutex`; this is because the mutex is not held
                // across `.await`, and in such cases Tokio documentation recommends doing so.
                let matches = Vec::new().pipe(Mutex::new).pipe(Arc::new);
                stream::iter(snippets).for_each_concurrent(None, |found| {
                    let all_matches = matches.clone();
                    async move {
                        if let Some(matches) = lookup_matches(client, concurrency_limiter, found).await {
                            total_count_matches.increment_by(matches.matching_snippets.len());
                            let mut combined = all_matches.lock().expect("lock is poisoned");
                            combined.push(matches);
                        }
                    }
                })
                .await;

                // The `Arc` was only cloned during the iteration above, so this _should_ not result in an error.
                // Since `Arc` explicitly moves reference counting to runtime this is an unavoidable check.
                // Similarly, we have to unwrap the `Mutex`, which was only used in the `Arc`;
                // the only time when this fails is if the `Mutex` is poisoned
                // (in which case panic is the only appropriate action).
                let matches = Arc::into_inner(matches).expect("all other references should be dropped");
                let matches = Mutex::into_inner(matches).expect("mutex is poisoned");

                // Once all the lookups are done, we write them all.
                let _permit = concurrency_limiter.acquire().await.expect("concurrency limiter has been closed");
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

async fn lookup_matches(
    client: &impl ApiClient,
    limiter: &Semaphore,
    found: ContentSnippet,
) -> Option<MatchingSnippet> {
    let _permit = limiter
        .acquire()
        .await
        .expect("concurrency limiter has been closed");

    let fingerprint = found.snippet().fingerprint();
    match client.lookup_snippets(fingerprint).await {
        Ok(matches) => {
            if matches.is_empty() {
                trace!(%fingerprint, "no matches in corpus");
                return None;
            }

            for matched in matches.iter() {
                trace!(%fingerprint, ?matched, "matched snippet");
            }

            MatchingSnippet::builder()
                .found_in(found.snippet().file_path())
                .local_snippet(found.snippet().clone())
                .local_text(String::from_utf8_lossy(found.content()))
                .matching_snippets(matches)
                .build()
                .pipe(Some)
        }
        Err(err) => {
            let err = Report::from(err);
            let file_path = found.snippet().file_path();
            let line_start = found.snippet().line_start();
            let line_end = found.snippet().line_end();
            warn!(%fingerprint, %file_path, %line_start, %line_end, "lookup snippet: {err:#}");
            None
        }
    }
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
