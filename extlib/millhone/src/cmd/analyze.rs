use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    str::FromStr,
};

use clap::Parser;
use getset::Getters;
use millhone::{
    api::prelude::*,
    extract::{ContentSnippet, Kind, Language},
};
use rayon::prelude::*;
use stable_eyre::{
    eyre::{bail, Context},
    Report,
};
use tap::Pipe;
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
pub fn main(endpoint: &BaseUrl, opts: Subcommand) -> Result<(), Report> {
    info!(
        api_key_id = %opts.auth.api_key_id(),
        output = %opts.output.display(),
        overwrite_output = %opts.overwrite_output,
        extract_opts = ?opts.extract,
        "analyzing local snippet matches",
    );

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
    let client = ApiClientV1::authenticated(endpoint, creds);
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
        // Resolve symlinks into full paths and filter to only files.
        .filter_map(|entry| -> Option<PathBuf> {
            debug!(path = %entry.path().display(), "resolve path");
            let path = super::resolve_path(&entry)
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
        })
        // Extract snippets from each file in parallel.
        .filter_map(|path| -> Option<(PathBuf, HashSet<ContentSnippet>)> {
            debug!(path = %path.display(), "extract snippets");
            let snippets = ContentSnippet::from_file(root, &snippet_opts, &path)
                .map_err(|err| warn!(path = %path.display(), "extract snippets: {err:#}"))
                .ok()?;

            if snippets.is_empty() {
                debug!(path = %path.display(), "no snippets extracted");
                return None;
            }

            let snippet_count = snippets.len();
            debug!(path = %path.display(), %snippet_count, "extracted snippets");
            total_count_snippets.increment_by(snippet_count);

            let snippets = snippets.into_iter().filter(|m| !snippet_is_noise(m)).collect::<HashSet<_>>();
            let snippet_count = snippets.len();
            debug!(path = %path.display(), %snippet_count, "filtered to non-noisy snippets");

            (path, snippets).pipe(Some)
        })
        // The goal is to then parallelize API calls, so flatten collections of snippets.
        // Using the serial `flat_map_iter` because this inner iterator has no computation aside from a clone.
        .flat_map_iter(|(path, snippets)| std::iter::repeat(path).zip(snippets))
        // Now match snippets up with the API into collections of matches.
        .filter_map(|(path, found)| -> Option<(PathBuf, MatchingSnippet)> {
            let fingerprint = found.snippet().fingerprint();
            let matching_snippets = client
                .lookup_snippets(fingerprint)
                .map_err(|err| warn!(path = %path.display(), %fingerprint, "lookup snippet: {err:#}"))
                .ok()?;
            if matching_snippets.is_empty() {
                trace!(%fingerprint, "no matches in corpus");
                return None;
            }
            for matched in matching_snippets.iter() {
                trace!(%fingerprint, ?matched, "matched snippet");
            }

            MatchingSnippet::builder()
                .found_in(found.snippet().file_path())
                .local_snippet(found.snippet().clone())
                .local_text(String::from_utf8_lossy(found.content()))
                .matching_snippets(matching_snippets)
                .build()
                .pipe(|record| (path, record))
                .pipe(Some)
        })
        // Snippet lookups were parallelized. Join them back together by path.
        // Need both fold and reduce; see https://docs.rs/rayon/latest/rayon/iter/trait.ParallelIterator.html#method.fold
        .fold(HashMap::new, |mut acc, (path, record)| {
            acc.entry(path).or_insert_with(Vec::new).push(record);
            acc
        })
        .reduce(HashMap::new, |mut acc, partial| {
            for (k, v) in partial {
                acc.entry(k).or_insert_with(Vec::new).extend(v);
            }
            acc
        })
        // Reduce handed back a hashmap. Spread it back out into an iterator,
        // such that each path corresponds to a single set of records.
        .into_par_iter()
        .for_each(|(path, records)| {
            let record_name = path
                .strip_prefix(root)
                .unwrap_or(&path)
                .to_string_lossy()
                .replace(std::path::MAIN_SEPARATOR_STR, "_");

            let current_ext = path.extension().and_then(|ext| ext.to_str()).unwrap_or("");
            let record_path = opts
                .output()
                .join(record_name)
                .with_extension(format!("{current_ext}.json"));

            let written = serde_json::to_string_pretty(&records)
                .context("encode records")
                .and_then(|encoded| std::fs::write(&record_path, encoded).context("write records"));
            match written {
                Ok(_) => info!(
                    "wrote {} matches for '{}' to '{}'",
                    records.len(),
                    path.display(),
                    record_path.display()
                ),
                Err(err) => warn!(record_path = %record_path.display(), "failed to write match records: {err:#}"),
            }
        });

    info!(
        total_count_entries = %total_count_entries.into_inner(),
        total_count_snippets = %total_count_snippets.into_inner(),
        total_count_files = %total_count_files.into_inner(),
        "finished extracting snippets",
    );

    Ok(())
}

/// Some snippets are just too noisy, for example basically every C program has an `int main`.
/// This function is meant to use to filter such snippets.
fn snippet_is_noise(m: &ContentSnippet) -> bool {
    let Ok(language) = Language::from_str(m.snippet().language()) else {
        warn!(snippet = ?m.snippet(), "unknown language, can't determine if noise");
        return false;
    };

    match language {
        Language::C => {
            m.snippet().kind() == &Kind::Signature.to_string()
                && contains_bytes(m.content(), b"int main")
        }
        Language::CPP => {
            m.snippet().kind() == &Kind::Signature.to_string()
                && contains_bytes(m.content(), b"int main")
        }
    }
}

fn contains_bytes(haystack: &[u8], needle: &[u8]) -> bool {
    haystack
        .windows(needle.len())
        .any(|window| window.eq(needle))
}
