use stable_eyre::Result;

fn main() -> Result<()> {
    let db = berkeleydb::open("testdata/centos5-plain/Packages".into())?;
    println!("Opened DB: {:?}", db.metadata);
    Ok(())
}
