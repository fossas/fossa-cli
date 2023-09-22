use std::{collections::HashSet, path::PathBuf};

use clap::{Parser, ValueEnum};
use getset::Getters;
use itertools::Itertools;
use millhone::{
    api::prelude::*,
    extract::{Kind, Target, Transform},
};
use stable_eyre::{eyre::Context, Report};
use strum::{Display, IntoEnumIterator};
use tracing::{info, warn};

use crate::cmd::MatchingSnippet;

/// Options for snippet ingestion.
#[derive(Debug, Parser, Getters)]
#[getset(get = "pub")]
#[clap(version)]
pub struct Subcommand {
    /// The path to the directory in which to write the `fossa-deps` file.
    ///
    /// If not specified, attempts to discover the most recent
    /// temporary directory created by the `analyze` subcommand.
    #[clap(long)]
    analyze_dir: Option<PathBuf>,

    /// The output format for the generated `fossa-deps` file.
    #[clap(long, default_value_t = OutputFormat::Json)]
    format: OutputFormat,

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
    /// The `fossa-deps` file is written into this directory.
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
pub fn main(endpoint: &BaseUrl, opts: Subcommand) -> Result<(), Report> {
    info!(
        analyze_dir = ?opts.analyze_dir,
        format = %opts.format,
        targets = ?opts.targets,
        kinds = ?opts.kinds,
        transforms = ?opts.transforms,
        "Committing local snippet matches",
    );

    let root = opts.target();
    let targets = default_if_empty(opts.targets(), Target::iter);
    let kinds = default_if_empty(opts.kinds(), Kind::iter);
    let transforms = default_if_empty(opts.transforms(), Transform::iter);

    Ok(())
}

fn default_if_empty<F, I, T>(items: &[T], or_default: F) -> HashSet<T>
where
    F: FnOnce() -> I,
    I: IntoIterator<Item = T>,
    T: Copy + std::hash::Hash + Eq,
{
    if items.is_empty() {
        or_default().into_iter().collect()
    } else {
        items.iter().copied().collect()
    }
}
