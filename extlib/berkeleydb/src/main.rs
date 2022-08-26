use std::path::PathBuf;

use berkeleydb::BerkeleyDB;
use clap::Parser;
use simple_logger::SimpleLogger;
use stable_eyre::{eyre::Context, Result};

#[derive(Parser, Debug)]
#[clap(version)]
struct Args {
    /// The BerkeleyDB file to extract.
    #[clap(short = 't', long)]
    target: PathBuf,

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

fn main() -> Result<()> {
    stable_eyre::install()?;
    let args = Args::parse();

    SimpleLogger::default()
        .with_level(args.log_level_filter())
        .init()?;

    let target = args.target;
    let mut db = BerkeleyDB::open(&target).wrap_err_with(|| format!("open db at '{target:?}'"))?;

    let values = db.read().context("read values from db")?;
    let encoded = serde_json::to_string(&values).context("encode values to JSON")?;
    println!("{encoded}");

    Ok(())
}
