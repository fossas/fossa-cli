use berkeleydb::{metadata::Page, BerkeleyDB};
use stable_eyre::Result;

#[test]
fn opens_db() -> Result<()> {
    let db = BerkeleyDB::open("./testdata/centos5-plain/Packages".into())?;
    let default = Page::default();
    assert_ne!(db.metadata, default);
    Ok(())
}
