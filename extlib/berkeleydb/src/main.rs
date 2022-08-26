use berkeleydb::BerkeleyDB;
use stable_eyre::{eyre::Context, Result};

fn main() -> Result<()> {
    let db = BerkeleyDB::open(
        "/Users/kit/projects/fossa-cli/extlib/berkeleydb/testdata/centos5-plain/Packages".into(),
    )
    .context("open db")?;
    println!("Opened DB: {:?}", db.metadata);

    let reader = db.read().context("open parser")?;
    for (read_values, value) in reader.into_iter().enumerate() {
        println!("Read value {read_values}: {value:?}");
    }

    Ok(())
}
