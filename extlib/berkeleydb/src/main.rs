use std::path::PathBuf;

use berkeleydb::BerkeleyDB;
use clap::Parser;
use log::debug;
use simple_logger::SimpleLogger;
use stable_eyre::{eyre::Context, Result};

/// Static builds on Linux suffer immensely with the default libc allocator,
/// but jemalloc suffers a lot less (~30% performance hit, compared to ~600%).
///
/// Through benchmarking, jemalloc appears to be roughly equivalent to libc,
/// so rather than deal with different feature flags for different systems
/// we'll just enable it unconditionally.
///
/// Reference: https://github.com/fossas/broker/blob/main/docs/dev/reference/static-binary.md
#[global_allocator]
static ALLOC: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

#[derive(Parser, Debug)]
#[clap(version)]
struct Args {
    /// The BerkeleyDB file to extract.
    /// Run with no target to parse file content from stdin (base64 encoded) instead.
    #[clap(short = 't', long)]
    target: Option<PathBuf>,

    /// Whether to emit debug logs.
    #[clap(short = 'd', long)]
    debug: bool,
}

impl Args {
    fn log_level_filter(&self) -> log::LevelFilter {
        if self.debug {
            log::LevelFilter::Debug
        } else {
            log::LevelFilter::Info
        }
    }
}

/// Open the database and output a JSON array of `base64` encoded [`berkeleydb::read::Value`] blobs.
/// These blobs are a shared format between different RPM backends, so the CLI contains a parser for the blob value.
fn main() -> Result<()> {
    stable_eyre::install()?;
    let args = Args::parse();

    SimpleLogger::default()
        .with_level(args.log_level_filter())
        .init()?;

    debug!("👩🏻‍💻 Starting up!");
    let mut db = if let Some(target) = args.target {
        debug!("📖 Reading database from file system: {target:?}");
        BerkeleyDB::open(&target).wrap_err_with(|| format!("open db at '{target:?}'"))?
    } else {
        debug!("📖 Reading database from stdin");
        BerkeleyDB::stdin().context("open db from stdin")?
    };

    let values = db.read().context("read values from db")?;
    let encoded = serde_json::to_string(&values).context("encode values to JSON")?;
    println!("{encoded}");

    debug!("✨ Values encoded as JSON to stdout; all done!");
    Ok(())
}
