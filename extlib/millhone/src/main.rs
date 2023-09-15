//! The frontend for the Millhone CLI.

#![deny(clippy::unwrap_used)]
#![deny(unsafe_code)]
#![warn(rust_2018_idioms)]

use clap::{Parser, Subcommand};
use millhone::url::BaseUrl;
use stable_eyre::eyre::Context;
use traceconf::TracingConfig;

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
    /// Tracing configuration.
    #[clap(flatten)]
    tracing: TracingConfig,

    /// The URL for the Millhone service.
    ///
    /// Note: In a future release we plan to move this to a reverse proxy through
    /// FOSSA's backend, similar to VSI functionality.
    /// At such time this argument will be hidden and only used for debugging,
    /// replaced with `endpoint`.
    #[clap(
        long,
        global = true,
        default_value = "https://api.millhone-staging.sherlock.fossa.team"
    )]
    direct_endpoint: BaseUrl,

    /// Subcommands for the CLI.
    #[clap(subcommand)]
    commands: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Ping the Millhone backend.
    Ping,

    /// Ingest snippets to the Millhone backend.
    // Boxed to reduce size difference between variants, a clippy lint.
    Ingest(Box<cmd::types::ingest::Options>),
}

fn main() -> stable_eyre::Result<()> {
    stable_eyre::install()?;
    let app = Application::parse();

    // Set up tracing according to the defaults.
    // We can customize this later if desired.
    let subscriber = app.tracing.subscriber();
    tracing::subscriber::set_global_default(subscriber).context("install tracing")?;

    // And then dispatch to the subcommand.
    match app.commands {
        Commands::Ping => cmd::ping::main(&app.direct_endpoint),
        Commands::Ingest(opts) => cmd::ingest::main(&app.direct_endpoint, *opts),
    }
}
