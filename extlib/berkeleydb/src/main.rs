use berkeleydb::BerkeleyDB;
use stable_eyre::{eyre::Context, Result};

fn main() -> Result<()> {
    let db = BerkeleyDB::open("testdata/centos5-plain/Packages".into()).context("open db")?;
    println!("Opened DB: {:?}", db.metadata);
    Ok(())
}
