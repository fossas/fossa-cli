use std::path::PathBuf;

use clap::Parser;
use regex::{Regex, RegexSet};
use stable_eyre::{eyre::Context, Result};
use tracing::{debug, info, trace, warn};
use walkdir::{DirEntry, WalkDir};

/// Arguments used by the "walk" command.
#[derive(Debug, Clone, Parser)]
#[command(version, about)]
pub struct WalkArgs {
    /// Don't walk paths that match any of the provided regular expressions.
    ///
    /// File paths that consist of invalid UTF-8 data are lossily converted
    /// using the unicode replacement character (U+FFFD).
    ///
    /// Note: this includes subdirectories;
    /// FOSSA CLI filtering is much more complicated so is not a 1:1 comparison.
    /// For more information, see: https://github.com/fossas/fossa-cli/blob/master/docs/contributing/filtering.md
    #[clap(long = "filter")]
    filters: Vec<Regex>,
}

#[tracing::instrument(skip(args), fields(root_device_id))]
pub fn main(root: PathBuf, args: &WalkArgs) -> Result<()> {
    info!("inspecting root: {}", root.display());
    trace!("inspecting root: {}", root.display());

    let root_device_id = util::device_num(&root).wrap_err("get device ID")?;
    tracing::Span::current().record("root_device_id", root_device_id);
    trace!("device_id: {root_device_id}");

    let regexes = args.filters.iter().map(|rx| rx.as_str());
    let filters = RegexSet::new(regexes).wrap_err("parse provided regular expressions as a set")?;
    debug!("user-provided filters: {filters:?}");

    let walker = WalkDir::new(&root)
        .follow_links(true)
        .into_iter()
        .filter_entry(|entry| {
            let path = entry.path().as_os_str().to_string_lossy().to_string();
            if filters.is_match(&path) {
                debug!(
                    "skipping due to user provided filter: {}",
                    entry.path().display()
                );
                false
            } else {
                true
            }
        })
        .filter_map(|entry| match entry {
            Ok(entry) => Some(entry),
            Err(err) => {
                warn!("unable to walk directory entry: {err:#}");
                None
            }
        });

    let mut total_entries = 0;
    let mut file_count = 0;
    let mut directory_count = 0;
    let mut deepest_level = (0, root.clone());
    for entry in walker {
        inspect_entry(root_device_id, &entry)
            .wrap_err_with(|| format!("inspect directory entry: {}", entry.path().display()))?;

        total_entries += 1;
        if entry.file_type().is_file() {
            file_count += 1;
        } else {
            directory_count += 1;
        }

        if entry.depth() > deepest_level.0 {
            deepest_level = (entry.depth(), entry.path().to_owned());
        }
    }

    info!(
        "entries inside '{}' inspected: {total_entries}",
        root.display(),
    );
    info!("directories: {directory_count}; files: {file_count}");
    info!(
        "deepest level: {} ('{}')",
        deepest_level.0,
        deepest_level.1.display()
    );

    Ok(())
}

#[tracing::instrument(skip_all, fields(entry = %entry.path().display(), depth = %entry.depth()))]
fn inspect_entry(root_device_id: u64, entry: &DirEntry) -> Result<()> {
    trace!("inspect: {}", entry.path().display());
    let metadata = entry.metadata().wrap_err("inspect metadata")?;

    trace!("is_dir: {}", metadata.is_dir());
    trace!("is_file: {}", metadata.is_file());
    trace!("is_symlink: {}", metadata.is_symlink());
    trace!("is_readonly: {}", metadata.permissions().readonly());

    let device_id = util::device_num(entry.path()).wrap_err("get device ID")?;
    trace!(
        "device_id: {device_id} (on root device: {})",
        root_device_id == device_id
    );

    Ok(())
}

#[cfg(windows)]
#[cfg(not(any(unix, windows)))]
mod util {
    use tracing::warn;

    pub fn device_num(path: &Path) -> Result<u64> {
        warn!(
            "unable to determine device for '{}': unsupported OS",
            path.display()
        );
        Ok(0)
    }
}

#[cfg(unix)]
mod util {
    use stable_eyre::eyre::Context;
    use stable_eyre::Result;
    use std::os::unix::fs::MetadataExt;
    use std::path::Path;

    pub fn device_num(path: &Path) -> Result<u64> {
        path.metadata()
            .map(|meta| meta.dev())
            .wrap_err_with(|| format!("get metadata for '{}'", path.display()))
    }
}

#[cfg(windows)]
mod util {
    use stable_eyre::Result;
    use winapi_util::{file, Handle};

    pub fn device_num(path: &Path) -> Result<u64> {
        let h = Handle::from_path_any(path)
            .wrap_err_with(|| format!("create system handle for '{}'", path.display()))?;
        file::information(h).map(|info| info.volume_serial_number())
    }
}
