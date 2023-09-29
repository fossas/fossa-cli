use std::{collections::HashSet, path::PathBuf};

use clap::{Parser, ValueEnum};
use getset::Getters;
use itertools::Itertools;
use millhone::extract::{Kind, Method, Target, Transform};
use serde::{Deserialize, Serialize};
use srclib::{Fetcher, Locator};
use stable_eyre::{
    eyre::{bail, eyre, Context},
    Report,
};
use strum::{Display, IntoEnumIterator};
use tap::{Pipe, Tap, TapFallible};
use tracing::{debug, error, info, warn};

use crate::cmd::MatchingSnippet;

/// Options for snippet ingestion.
#[derive(Debug, Parser, Getters)]
#[getset(get = "pub")]
#[clap(version)]
pub struct Subcommand {
    /// The path to the directory in which the `analyze` subcommand wrote its output.
    #[clap(long)]
    analyze_output_dir: PathBuf,

    /// The output format for the generated `fossa-deps` file.
    #[clap(long, default_value_t = OutputFormat::Json)]
    format: OutputFormat,

    /// If specified, overwrites the output file if it exists.
    #[clap(long)]
    overwrite_fossa_deps: bool,

    /// Only commit matches with the specified targets.
    ///
    /// Defaults to all supported targets.
    /// Specify the argument multiple times to indicate additional options.
    #[clap(long = "target")]
    targets: Vec<Target>,

    /// Only commit matches with the specified kinds.
    ///
    /// Defaults to all supported kinds.
    /// Specify the argument multiple times to indicate additional options.
    #[clap(long = "kind")]
    kinds: Vec<Kind>,

    /// Only commit matches with the specified transforms.
    ///
    /// Defaults to all supported transforms. Raw matches (i.e. no transforms)
    /// are always committed and cannot be disabled.
    /// Specify the argument multiple times to indicate additional options.
    #[clap(long = "transform")]
    transforms: Vec<Transform>,

    /// The directory being analyzed by FOSSA.
    ///
    /// The `fossa-deps` file is written into the root of this directory.
    target: PathBuf,
}

/// The output format for the generated `fossa-deps` file.
#[derive(Debug, Clone, Copy, ValueEnum, Display)]
#[strum(serialize_all = "snake_case")]
pub enum OutputFormat {
    /// Commit as `fossa-deps.json`.
    Json,

    /// Commit as `fossa-deps.yml`.
    Yml,
}

#[tracing::instrument(skip_all, fields(target = %opts.target().display()))]
pub fn main(opts: Subcommand) -> Result<(), Report> {
    info!(
        analyze_output_dir = ?opts.analyze_output_dir,
        format = %opts.format,
        targets = ?opts.targets,
        kinds = ?opts.kinds,
        transforms = ?opts.transforms,
        "Committing local snippet matches",
    );

    let output_file = opts
        .target()
        .join("fossa-deps")
        .with_extension(opts.format().to_string());
    if !opts.overwrite_fossa_deps() && output_file.exists() {
        bail!(
            "The output file '{}' already exists. This program does not currently merge `fossa-deps` files.",
            output_file.display(),
        );
    }

    if !opts.analyze_output_dir().exists() {
        bail!(
            "The directory specified for `analyze` output, '{}', does not exist.",
            opts.analyze_output_dir().display()
        );
    }

    let match_targets = default_if_empty(opts.targets(), Target::iter);
    let match_kinds = default_if_empty(opts.kinds(), Kind::iter);
    let match_methods = if opts.transforms().is_empty() {
        Method::iter()
            .map(|m| m.to_string())
            .collect::<HashSet<_>>()
    } else {
        opts.transforms()
            .iter()
            .copied()
            .pipe(Method::iter_constrained)
            .map(|m| m.to_string())
            .collect::<HashSet<_>>()
    };

    let deps = std::fs::read_dir(opts.analyze_output_dir())
        .wrap_err_with(|| format!("list contents of '{}'", opts.analyze_output_dir().display()))?
        .filter_map(|entry| entry.tap_err(|err| error!(analyze_output_dir = %opts.analyze_output_dir().display(), "walk contents: {err:#}")).ok())
        .inspect(|entry| debug!(analyze_output_dir = %opts.analyze_output_dir().display(), ?entry, "walk contents"))
        .filter(|entry| {
            let path = entry.path();
            path.extension()
                .and_then(|ext| ext.to_str())
                .map(|ext| ext == "json")
                .unwrap_or_default()
                .tap(|valid| debug!(path = %entry.path().display(), %valid, "filter to json"))
        })
        .filter_map(|entry| {
            std::fs::read(entry.path())
                .wrap_err_with(|| format!("read '{}'", entry.path().display()))
                .tap_ok(|_| debug!(path = %entry.path().display(), "read entry"))
                .tap_err(|err| debug!(path = %entry.path().display(), "read entry: {err:#}"))
                .map(|content| (entry, content))
                .ok()
        })
        .filter_map(|(entry, content)| {
            serde_json::from_slice::<Vec<MatchingSnippet>>(&content)
                .tap_err(|err| debug!(path = %entry.path().display(), "read entry: {err:#}"))
                .ok()
        })
        .flatten()
        .flat_map(|m| m.matching_snippets)
        .filter_map(|m| {
            let matches = match_kinds.contains(m.snippet().kind())
                && match_targets.contains(m.snippet().target())
                && match_methods.contains(m.snippet().method());
            debug!(
                ?match_kinds,
                ?match_targets,
                ?match_methods,
                "snippet {m:?} matches criteria: {matches}"
            );

            if matches {
                m.locator().clone().pipe(Some)
            } else {
                None
            }
        })
        .unique()
        .sorted_by_key(|loc| loc.to_string())
        .filter_map(|loc| {
            ReferencedDependency::try_from(loc)
                .tap_err(|(loc, err)| {
                    warn!("unable to translate locator '{loc}' to referenced dependency: {err:#}")
                })
                .ok()
        })
        .collect_vec();

    if deps.is_empty() {
        info!("No dependencies committed");
        return Ok(());
    }

    let deps_file = FossaDeps { referenced: deps };
    let rendered = match opts.format {
        OutputFormat::Json => {
            serde_json::to_string_pretty(&deps_file).context("render fossa-deps")?
        }
        OutputFormat::Yml => serde_yaml::to_string(&deps_file).context("render fossa-deps")?,
    };
    std::fs::write(&output_file, rendered)
        .wrap_err_with(|| format!("write fossa-deps at '{}'", output_file.display()))?;

    info!(
        "Wrote output to '{}' with dependency count: {}",
        output_file.display(),
        deps_file.referenced.len(),
    );
    Ok(())
}

fn default_if_empty<F, I, T>(items: &[T], or_default: F) -> HashSet<String>
where
    F: FnOnce() -> I,
    I: IntoIterator<Item = T>,
    T: ToString,
{
    if items.is_empty() {
        or_default().into_iter().map(|s| s.to_string()).collect()
    } else {
        items.iter().map(|s| s.to_string()).collect()
    }
}

/// Serialize the entries of the `referenced-dependencies` type in `fossa-deps`:
/// https://github.com/fossas/fossa-cli/blob/master/docs/references/files/fossa-deps.md#referenced-dependencies
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct ReferencedDependency {
    #[serde(rename = "type")]
    kind: String,
    name: String,
    version: String,
}

impl ReferencedDependency {
    fn kind_for_fetcher(fetcher: Fetcher) -> Result<String, Report> {
        match fetcher {
            Fetcher::Git => Ok("git".into()),
            _ => Err(eyre!("unsupported fetcher: {fetcher}")),
        }
    }
}

impl TryFrom<Locator> for ReferencedDependency {
    type Error = (Locator, Report);

    fn try_from(loc: Locator) -> Result<Self, Self::Error> {
        let kind = Self::kind_for_fetcher(loc.fetcher())
            .context("translate fetcher to 'referenced-dependency'.type")
            .map_err(|err| (loc.clone(), err))?;
        let version = loc
            .revision()
            .as_ref()
            .cloned()
            .ok_or_else(|| (loc.clone(), eyre!("version required for locator")))?;

        Ok(Self {
            kind,
            version,
            name: loc.project().to_string(),
        })
    }
}

/// Serialize a `fossa-deps` file.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct FossaDeps {
    #[serde(rename = "referenced-dependencies")]
    referenced: Vec<ReferencedDependency>,
}
