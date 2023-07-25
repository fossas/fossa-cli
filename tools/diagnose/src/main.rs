use clap::Parser;
use cmd::{Commands, Format, Opts};
use stable_eyre::{eyre::Context, Result};
use tracing_subscriber::{prelude::*, Layer, Registry};

mod cmd;

fn main() -> Result<()> {
    stable_eyre::install()?;

    let opts = Opts::parse();
    let root = opts.dir().wrap_err("determine scan root")?;

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

    match opts.command() {
        Commands::Walk(args) => cmd::walk::main(root, args),
        Commands::DebugOutputFormat => cmd::debug_output_format::main(),
    }
}
