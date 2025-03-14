use clap::Parser;
use cmd::{Commands, Format, Mode, Opts};
use stable_eyre::{eyre::Context, Result};
use tracing_subscriber::{prelude::*, Layer, Registry};

mod cmd;

/// Static builds on Linux suffer immensely with the default libc allocator,
/// but jemalloc suffers a lot less (~30% performance hit, compared to ~600%).
///
/// Reference: https://github.com/fossas/broker/blob/main/docs/dev/reference/static-binary.md
#[cfg(feature = "jemalloc")]
#[global_allocator]
static ALLOC: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

fn main() -> Result<()> {
    stable_eyre::install()?;

    let opts = Opts::parse();
    let root = opts.dir().wrap_err("determine scan root")?;

    if matches!(opts.mode(), Mode::Log) {
        match opts.format() {
            Format::Text => tracing::subscriber::set_global_default(
                Registry::default().with(
                    tracing_subscriber::fmt::layer()
                        .with_file(false)
                        .with_line_number(false)
                        .with_span_events(opts.fmt_span())
                        .with_filter(opts.level_filter()),
                ),
            ),
            Format::Json => tracing::subscriber::set_global_default(
                Registry::default().with(
                    tracing_subscriber::fmt::layer()
                        .json()
                        .with_span_events(opts.fmt_span())
                        .with_filter(opts.level_filter()),
                ),
            ),
        }
        .wrap_err("set tracing subscriber")?;
    }

    match opts.command() {
        Commands::Walk(args) => cmd::walk::main(root, opts.mode(), args),
        Commands::DebugOutputFormat => cmd::debug_output_format::main(),
    }
}
