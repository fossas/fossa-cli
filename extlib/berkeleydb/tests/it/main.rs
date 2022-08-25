use std::path::PathBuf;

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
fn reads_centos5_plain() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos5-plain/Packages")
}

#[test]
fn reads_centos6_devtools() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos6-devtools/Packages")
}

#[test]
fn reads_centos6_many() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos6-many/Packages")
}

#[test]
fn reads_centos6_plain() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos6-plain/Packages")
}

#[test]
fn reads_centos7_devtools() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos7-devtools/Packages")
}

#[test]
fn reads_centos7_httpd24() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos7-httpd24/Packages")
}

#[test]
fn reads_centos7_many() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos7-many/Packages")
}

#[test]
fn reads_centos7_plain() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos7-plain/Packages")
}

#[test]
fn reads_centos7_python35() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos7-python35/Packages")
}

#[test]
fn reads_centos8_modularitylabel() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos8-modularitylabel/Packages")
}

#[test]
fn reads_ubi8_s390x() -> Result<()> {
    assert_reads_nonzero_values("./testdata/ubi8-s390x/Packages")
}

#[track_caller]
fn assert_reads_nonzero_values(path: impl Into<PathBuf>) -> Result<()> {
    let db = BerkeleyDB::open(path.into())?;
    let read_values = db
        .read()
        .context("open reader")?
        .map(|value| value.expect("must read value"))
        .map(|value| {
            assert_ne!(value, Value::default());
            value
        })
        .count();
    assert_ne!(read_values, 0);
    Ok(())
}
