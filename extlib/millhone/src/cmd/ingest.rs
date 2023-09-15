use std::{borrow::Cow, collections::HashSet, fs, path::Path};

use millhone::api::{prelude::*, Credentials, Snippet};
use snippets::{language::c99_tc3, Extractor};
use srclib::Locator;
use stable_eyre::eyre::Context;
use tap::Pipe;
use tracing::{debug, info, warn};
use uuid::Uuid;
use walkdir::{DirEntry, WalkDir};

use super::types::ingest::Options;

#[tracing::instrument(skip_all, fields(target = %opts.target().display()))]
pub fn main(endpoint: &BaseUrl, opts: Options) -> stable_eyre::Result<()> {
    let ingest_id = opts
        .ingest_id()
        .as_ref()
        .cloned()
        .unwrap_or_else(|| Uuid::new_v4().to_string());

    info!(
        %ingest_id,
        api_key_id = %opts.api_key_id(),
        locator = %opts.locator(),
        "Ingesting snippets",
    );

    let snippet_opts = snippets::Options::new(
        opts.targets().iter().copied().map(Into::into),
        opts.kinds().iter().copied().map(Into::into),
        opts.transforms().iter().copied().map(Into::into),
    );

    let creds = Credentials::new(opts.api_key_id().clone(), opts.api_secret().clone());
    let client = ApiClientV1::authenticated(endpoint, creds);
    let walk = WalkDir::new(opts.target())
        // Follow symlinks; loops are yielded as errors automatically.
        .follow_links(true)
        // Not chosen for a specific reason, just seems reasonable.
        .max_depth(1000)
        // Just make the walk deterministic (per directory anyway).
        .sort_by_file_name();

    let mut total_count_entries = 0usize;
    let mut total_count_snippets = 0usize;
    let mut total_count_files = 0usize;
    for entry in walk.into_iter() {
        total_count_entries += 1;
        let Some(entry) = unwrap_entry(entry) else {
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
        let snippets = extract(&ingest_id, opts.locator(), &snippet_opts, &path)
            .wrap_err_with(|| format!("process '{}'", path.display()))?;

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

#[tracing::instrument]
fn extract(
    ingest_id: &str,
    locator: &Locator,
    opts: &snippets::Options,
    path: &Path,
) -> stable_eyre::Result<HashSet<Snippet>> {
    let ext = match path.extension() {
        None => {
            debug!("skipping: no extension");
            return Ok(Default::default());
        }
        Some(ext) => ext.to_string_lossy(),
    };

    let content = fs::read(path).context("read file")?;
    if ext == "c" {
        c99_tc3::Extractor::extract(opts, &content)
            .context("extract snippets")?
            .into_iter()
            .map(|snippet| Snippet::from(ingest_id, locator, path, &content, snippet))
            .collect::<HashSet<_>>()
            .pipe(Ok)
    } else {
        debug!(%ext, "skipping: ext unsupported");
        Ok(Default::default())
    }
}

#[tracing::instrument]
fn unwrap_entry(entry: Result<DirEntry, walkdir::Error>) -> Option<DirEntry> {
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
