use clap::Parser;
use cmd::{Commands, Format, Opts};
use stable_eyre::{eyre::Context, Result};
use tracing::{debug, error, info, trace, warn};
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
        Commands::DebugOutputFormat => debug_output_format(),
    }
}

fn debug_output_format() -> Result<()> {
    #[tracing::instrument]
    fn inner(value: &str) {
        error!("message at level 'Error' inside function 'inner'");
        info!("message at level 'Info' inside function 'inner'");
        warn!("message at level 'Warn' inside function 'inner'");
        debug!("message at level 'Debug' inside function 'inner'");
        trace!("message at level 'Trace' inside function 'inner'");
        let _ = value;
        nested_inner("some value passed to 'nested_inner'")
    }

    #[tracing::instrument]
    fn nested_inner(value: &str) {
        error!("message at level 'Error' inside function 'nested_inner_function', called from inside 'inner'");
        info!("message at level 'Info' inside function 'nested_inner_function', called from inside 'inner'");
        warn!("message at level 'Warn' inside function 'nested_inner_function', called from inside 'inner'");
        debug!("message at level 'Debug' inside function 'nested_inner_function', called from inside 'inner'");
        trace!("message at level 'Trace' inside function 'nested_inner_function', called from inside 'inner'");
        let _ = value;
    }

    #[tracing::instrument(fields(uppercased))]
    fn uppercase(input: &str) -> String {
        let result = input.to_uppercase();
        tracing::Span::current().record("uppercased", &result);
        result
    }

    info!("demonstrating how messages are logged.");
    info!("");
    error!("message at level 'Error'");
    info!("message at level 'Info'");
    warn!("message at level 'Warn'");
    debug!("message at level 'Debug'");
    trace!("message at level 'Trace'");

    let target = "hello";
    info!("");
    info!("-------");
    info!("demonstrating how spans are logged.");
    info!("calling 'uppercase' with the argument '{target}', then logging the result:");
    info!("");
    info!("the word '{target}' uppercased is...");
    let result = uppercase(target);
    info!("> '{result}'");

    info!("");
    info!("-------");
    info!("demonstrating how nested spans are logged.");
    info!("calling the function 'inner' with an argument, which will in turn call 'nested_inner' with a different argument.");
    info!("");
    inner("some value passed to 'inner'");

    Ok(())
}
