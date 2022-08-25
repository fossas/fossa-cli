use berkeleydb::{metadata::Page, read::value::Value, BerkeleyDB};
use stable_eyre::{eyre::Context, Result};

#[test]
fn opens_db() -> Result<()> {
    let db = BerkeleyDB::open("./testdata/centos5-plain/Packages".into())?;
    let default = Page::default();
    assert_ne!(db.metadata, default);
    Ok(())
}

#[test]
fn reads_items() -> Result<()> {
    let db = BerkeleyDB::open("./testdata/centos5-plain/Packages".into())?;

    let mut read_values = 0;
    for value in db.read().context("open parser")? {
        let value = value.context("read value")?;
        let default = Value::default();
        assert_ne!(value, default);
        read_values += 1;
    }

    assert_ne!(read_values, 0);
    Ok(())
}
