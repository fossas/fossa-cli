//! The frontend for the Millhone CLI.

#![deny(clippy::unwrap_used)]
#![deny(unsafe_code)]
#![warn(rust_2018_idioms)]

use clap::{Parser, Subcommand};
use millhone::url::BaseUrl;
use stable_eyre::eyre::Context;
use tap::Pipe;
use traceconf::{Colors, Format, Level};
use tracing_subscriber::{filter::filter_fn, prelude::*};

mod cmd;

/// Static builds on Linux suffer immensely with the default libc allocator,
/// but jemalloc suffers a lot less (~30% performance hit, compared to ~600%).
///
/// Reference: https://github.com/fossas/broker/blob/main/docs/dev/reference/static-binary.md
#[cfg(feature = "jemalloc")]
#[global_allocator]
static ALLOC: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

/// Millhone is the CLI for FOSSA's snippet scanning.
#[derive(Debug, Parser)]
#[clap(version)]
struct Application {
    /// Set the minimum level for logs. Logs below this level are dropped.
    #[clap(long, default_value_t = Level::default())]
    log_level: Level,

    /// The formatter to use for logs.
    #[clap(long, default_value_t = Format::default())]
    log_format: Format,

    /// The coloring mode to use for log and span traces.
    #[clap(long, default_value_t = Colors::default())]
    log_colors: Colors,

    /// The URL for the Millhone service.
    ///
    /// Note: In a future release we plan to move this to a reverse proxy through
    /// FOSSA's backend, similar to VSI functionality.
    /// At such time this argument will be hidden and only used for debugging,
    /// replaced with `endpoint`.
    #[clap(long, global = true)]
    #[cfg_attr(
        debug_assertions,
        clap(default_value = "https://api.millhone-staging.sherlock.fossa.team")
    )]
    #[cfg_attr(
        not(debug_assertions),
        clap(default_value = "https://api.millhone-prod.sherlock.fossa.team")
    )]
    direct_endpoint: BaseUrl,

    /// Subcommands for the CLI.
    #[clap(subcommand)]
    commands: Commands,
}

impl Application {
    /// Determine whether colors should be enabled for tracing output.
    pub fn colors_enabled(&self) -> bool {
        match self.log_colors {
            Colors::Auto => atty::is(atty::Stream::Stderr),
            Colors::Enable => true,
            _ => false,
        }
    }

    /// Translate the level selected by the user into the format used by [`tracing`].
    pub fn level_filter(&self) -> tracing_subscriber::filter::LevelFilter {
        self.log_level.into()
    }
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Ping the Millhone backend.
    Ping,

    /// Ingest snippets to the Millhone backend.
    // Boxed to reduce size difference between variants, a clippy lint.
    Ingest(cmd::ingest::Subcommand),

    /// Analyze a local project for matches.
    Analyze(cmd::analyze::Subcommand),

    /// Commit matches discovered during analyze into a fossa-deps file.
    Commit(cmd::commit::Subcommand),
}

fn main() -> stable_eyre::Result<()> {
    stable_eyre::install()?;
    let app = Application::parse();

    match app.log_format {
        Format::Json => tracing_subscriber::Registry::default()
            .with(
                tracing_subscriber::fmt::layer()
                    .json()
                    .with_ansi(app.colors_enabled())
                    .with_writer(std::io::stdout)
                    .with_span_events(tracing_subscriber::fmt::format::FmtSpan::ACTIVE)
                    .with_filter(app.level_filter())
                    .with_filter(self_sourced_events(app.log_level)),
            )
            .pipe(tracing::subscriber::set_global_default)
            .context("install tracing")?,
        _ => tracing_subscriber::Registry::default()
            .with(
                tracing_subscriber::fmt::layer()
                    .with_ansi(app.colors_enabled())
                    .with_writer(std::io::stdout)
                    .with_file(false)
                    .with_line_number(false)
                    .without_time()
                    .with_target(false)
                    .with_span_events(tracing_subscriber::fmt::format::FmtSpan::NONE)
                    .with_filter(app.level_filter())
                    .with_filter(self_sourced_events(app.log_level)),
            )
            .pipe(tracing::subscriber::set_global_default)
            .context("install tracing")?,
    }

    // And then dispatch to the subcommand.
    match app.commands {
        Commands::Ping => cmd::ping::main(&app.direct_endpoint),
        Commands::Ingest(opts) => cmd::ingest::main(&app.direct_endpoint, opts),
        Commands::Analyze(opts) => cmd::analyze::main(&app.direct_endpoint, opts),
        Commands::Commit(opts) => cmd::commit::main(opts),
    }
}

/// Filters traces from other crates if trace log level isn't set.
fn self_sourced_events<S>(log_level: Level) -> impl tracing_subscriber::layer::Filter<S> {
    const CURRENT_CRATE: &str = env!("CARGO_PKG_NAME");
    fn is_current_crate(meta: &tracing::Metadata<'_>) -> bool {
        meta.target().starts_with(CURRENT_CRATE)
    }

    let enable_debug_logging = log_level >= Level::Trace;
    filter_fn(move |meta| is_current_crate(meta) || enable_debug_logging)
}
