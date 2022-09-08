use std::{
    fs::{self, File},
    io,
    path::PathBuf,
};

use bdb::{metadata::Page, read::value::Value, BerkeleyDB};
use lexical_sort::{natural_lexical_cmp, StringSort};
use stable_eyre::{eyre::Context, Result};

macro_rules! read_db {
    ($path:expr) => {{
        let mut f = File::open($path).context("open file")?;
        let mut buf = Vec::new();
        io::copy(&mut f, &mut buf).context("read file")?;
        BerkeleyDB::from(buf).context("parse db")
    }};
}

#[test]
fn opens_db() -> Result<()> {
    let db = read_db!("./testdata/centos5-plain/Packages")?;
    let default = Page::default();
    assert_ne!(db.metadata, default);
    Ok(())
}

#[test]
fn reads_centos5_plain_nonzero() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos5-plain")
}

#[test]
fn reads_centos6_devtools_nonzero() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos6-devtools")
}

#[test]
fn reads_centos6_many_nonzero() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos6-many")
}

#[test]
fn reads_centos6_plain_nonzero() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos6-plain")
}

#[test]
fn reads_centos7_devtools_nonzero() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos7-devtools")
}

#[test]
fn reads_centos7_httpd24_nonzero() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos7-httpd24")
}

#[test]
fn reads_centos7_many_nonzero() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos7-many")
}

#[test]
fn reads_centos7_plain_nonzero() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos7-plain")
}

#[test]
fn reads_centos7_python35_nonzero() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos7-python35")
}

#[test]
fn reads_centos8_modularitylabel_nonzero() -> Result<()> {
    assert_reads_nonzero_values("./testdata/centos8-modularitylabel")
}

#[test]
fn reads_ubi8_s390x_nonzero() -> Result<()> {
    assert_reads_nonzero_values("./testdata/ubi8-s390x")
}

#[test]
fn reads_centos5_plain_expected() -> Result<()> {
    assert_reads_expected("./testdata/centos5-plain")
}

#[test]
fn reads_centos6_devtools_expected() -> Result<()> {
    assert_reads_expected("./testdata/centos6-devtools")
}

#[test]
fn reads_centos6_many_expected() -> Result<()> {
    assert_reads_expected("./testdata/centos6-many")
}

#[test]
fn reads_centos6_plain_expected() -> Result<()> {
    assert_reads_expected("./testdata/centos6-plain")
}

#[test]
fn reads_centos7_devtools_expected() -> Result<()> {
    assert_reads_expected("./testdata/centos7-devtools")
}

#[test]
fn reads_centos7_httpd24_expected() -> Result<()> {
    assert_reads_expected("./testdata/centos7-httpd24")
}

#[test]
fn reads_centos7_many_expected() -> Result<()> {
    assert_reads_expected("./testdata/centos7-many")
}

#[test]
fn reads_centos7_plain_expected() -> Result<()> {
    assert_reads_expected("./testdata/centos7-plain")
}

#[test]
fn reads_centos7_python35_expected() -> Result<()> {
    assert_reads_expected("./testdata/centos7-python35")
}

#[test]
fn reads_centos8_modularitylabel_expected() -> Result<()> {
    assert_reads_expected("./testdata/centos8-modularitylabel")
}

#[test]
fn reads_ubi8_s390x_expected() -> Result<()> {
    assert_reads_expected("./testdata/ubi8-s390x")
}

#[track_caller]
fn assert_reads_nonzero_values(path: impl Into<PathBuf>) -> Result<()> {
    let mut db = read_db!(path.into().join("Packages"))?;

    let default = Value::default();
    let read_values = db
        .read()
        .context("open reader")?
        .into_iter()
        .map(|value| {
            assert_ne!(default, value);
            value
        })
        .count();
    assert_ne!(read_values, 0);
    Ok(())
}

// Created bins with the following commands using https://github.com/jssblck/go-rpmdb/tree/directed-bin-dir
// These commands were run in the context of the `go-rpmdb` repo.
//
// ; BIN_DIR=~/projects/fossa-cli/extlib/berkeleydb/testdata/centos5-plain/bins go run cmd/rpmdb/main.go pkg/testdata/centos5-plain/Packages
// ; BIN_DIR=~/projects/fossa-cli/extlib/berkeleydb/testdata/centos6-devtools/bins go run cmd/rpmdb/main.go pkg/testdata/centos6-devtools/Packages
// ; BIN_DIR=~/projects/fossa-cli/extlib/berkeleydb/testdata/centos6-many/bins go run cmd/rpmdb/main.go pkg/testdata/centos6-many/Packages
// ; BIN_DIR=~/projects/fossa-cli/extlib/berkeleydb/testdata/centos6-plain/bins go run cmd/rpmdb/main.go pkg/testdata/centos6-plain/Packages
// ; BIN_DIR=~/projects/fossa-cli/extlib/berkeleydb/testdata/centos7-devtools/bins go run cmd/rpmdb/main.go pkg/testdata/centos7-devtools/Packages
// ; BIN_DIR=~/projects/fossa-cli/extlib/berkeleydb/testdata/centos7-httpd24/bins go run cmd/rpmdb/main.go pkg/testdata/centos7-httpd24/Packages
// ; BIN_DIR=~/projects/fossa-cli/extlib/berkeleydb/testdata/centos7-many/bins go run cmd/rpmdb/main.go pkg/testdata/centos7-many/Packages
// ; BIN_DIR=~/projects/fossa-cli/extlib/berkeleydb/testdata/centos7-plain/bins go run cmd/rpmdb/main.go pkg/testdata/centos7-plain/Packages
// ; BIN_DIR=~/projects/fossa-cli/extlib/berkeleydb/testdata/centos7-python35/bins go run cmd/rpmdb/main.go pkg/testdata/centos7-python35/Packages
// ; BIN_DIR=~/projects/fossa-cli/extlib/berkeleydb/testdata/centos8-modularitylabel/bins go run cmd/rpmdb/main.go pkg/testdata/centos8-modularitylabel/Packages
// ; BIN_DIR=~/projects/fossa-cli/extlib/berkeleydb/testdata/ubi8-s390x/bins go run cmd/rpmdb/main.go pkg/testdata/ubi8-s390x/Packages

#[track_caller]
fn assert_reads_expected(path: impl Into<PathBuf>) -> Result<()> {
    let path = path.into();
    let mut db = read_db!(path.join("Packages"))?;

    let bins_dir = path.join("bins");
    let mut files = fs::read_dir(&bins_dir)?
        .map(|e| e.expect("must read dir"))
        .map(|e| e.file_name().to_string_lossy().to_string())
        .collect::<Vec<_>>();
    files.string_sort_unstable(natural_lexical_cmp);
    let bins = files
        .into_iter()
        .map(|n| (n.clone(), bins_dir.join(n)))
        .map(|(n, p)| (n, fs::read(p).expect("must read file")));

    // We don't parse any data out of the actual blobs. Instead, we just check that we read the same blobs in the same order.
    let mut read_any = false;
    let reader = db.read()?.into_iter().map(|v| v.into_inner());
    for (idx, (value, (name, expected))) in reader.zip(bins).enumerate() {
        assert_eq!(expected, value, "item {idx} ('{name}') must match");
        read_any = true;
    }

    assert!(read_any);
    Ok(())
}
