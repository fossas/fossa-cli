use berkeleydb::metadata::Page;
use stable_eyre::Result;

#[test]
fn opens_db() -> Result<()> {
    let db = berkeleydb::open("./testdata/centos5-plain/Packages".into())?;
    let default = Page::default();
    assert_ne!(db.metadata, default);
    Ok(())
}
