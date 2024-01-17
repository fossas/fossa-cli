use clap::Parser;
use log::debug;
use reachability::Reachability;
use simple_logger::SimpleLogger;
use stable_eyre::{eyre::Context, Result};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[clap(version)]
struct Args {
    /// Target file or directory from which to perform call paths extraction.
    /// Run with no target to parse file content from stdin instead.
    #[clap(long)]
    target: Option<PathBuf>,

    /// Whether to emit debug logs.
    #[clap(short = 'd', long)]
    debug: bool,
}

impl Args {
    fn log_level_filter(&self) -> log::LevelFilter {
        if self.debug {
            return log::LevelFilter::Debug;
        }

        log::LevelFilter::Info
    }
}

fn main() -> Result<()> {
    let args = Args::parse();
    SimpleLogger::default()
        .with_level(args.log_level_filter())
        .init()?;

    // Perform extraction
    let call_graph = if let Some(target) = args.target {
        debug!("Reading from file system: {target:?}");
        Reachability::from_fs(&target)
    } else {
        debug!("Reading from stdin");
        Reachability::from_stdin()
    }?;

    let encoded = serde_json::to_string(&call_graph).context("encode values to JSON")?;
    println!("{encoded}");

    debug!("Values encoded as JSON to stdout; all done!");
    Ok(())
}
