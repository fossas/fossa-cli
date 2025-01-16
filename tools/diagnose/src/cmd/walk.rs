use std::{
    path::PathBuf,
    time::{Duration, Instant},
};

use bounded_vec_deque::BoundedVecDeque;
use clap::{Parser, ValueEnum};
use ratatui::{
    crossterm::event::{self, Event, KeyCode, KeyEventKind},
    layout::{Constraint, Layout},
    widgets::{Block, List, Paragraph},
    DefaultTerminal,
};
use regex::{Regex, RegexSet};
use stable_eyre::{eyre::Context, Result};
use strum::Display;
use tracing::{debug, info, trace, warn};
use walkdir::{DirEntry, WalkDir};

use super::Mode;

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

    /// Control whether symbolic links (symlinks) are followed.
    ///
    /// FOSSA CLI follows symlinks by default, so this is defaulted to following symlinks.
    /// Broken or looping symlinks are warned and then ignored.
    #[clap(long, default_value_t = SymlinkStrategy::Follow)]
    symlinks: SymlinkStrategy,
}

/// The strategy for how to handle symbolic links while walking the file system.
#[derive(Debug, Clone, Copy, ValueEnum, Display)]
pub enum SymlinkStrategy {
    /// Symbolic links are followed.
    #[strum(serialize = "follow")]
    Follow,

    /// Symbolic links are ignored.
    #[strum(serialize = "skip")]
    Skip,
}

#[tracing::instrument(skip(args), fields(root_device_id))]
pub fn main(root: PathBuf, mode: Mode, args: &WalkArgs) -> Result<()> {
    match mode {
        Mode::Log => main_logged(root, args),
        Mode::Tui => {
            let start = Instant::now();
            let mut terminal = ratatui::init();
            let result = main_tui(root, args, &mut terminal);
            ratatui::restore();

            match result {
                Ok(stats) => {
                    println!("walk stats:");
                    println!("  skipped: {}", stats.skipped);
                    println!("  warnings: {}", stats.warnings);
                    println!("  loops: {}", stats.loops);
                    println!("  visited: {}", stats.visited);
                    println!("  time: {:?}", start.elapsed());
                    Ok(())
                }
                Err(err) => Err(err),
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct WalkStats {
    skipped: usize,
    warnings: usize,
    loops: usize,
    visited: usize,
}

#[tracing::instrument(skip(args), fields(root_device_id))]
fn main_tui(root: PathBuf, args: &WalkArgs, terminal: &mut DefaultTerminal) -> Result<WalkStats> {
    let regexes = args.filters.iter().map(|rx| rx.as_str());
    let filters = RegexSet::new(regexes).wrap_err("parse provided regular expressions as a set")?;

    let mut skipped_count = 0usize;
    let mut warnings_count = 0usize;
    let mut loops_count = 0usize;
    let mut visited_count = 0usize;
    let mut skipped = BoundedVecDeque::new(1000);
    let mut warnings = BoundedVecDeque::new(1000);
    let mut loops = BoundedVecDeque::new(1000);
    let mut visited = BoundedVecDeque::new(1000);
    let mut drawn = Instant::now();

    const FPS: u64 = 30;
    const TIME_PER_FRAME: Duration = Duration::from_millis(1000 / FPS);

    let walker = WalkDir::new(&root)
        .follow_links(matches!(args.symlinks, SymlinkStrategy::Follow))
        .into_iter();

    for entry in walker {
        let path = match entry {
            Ok(entry) => entry.path().to_string_lossy().to_string(),
            Err(err) => {
                let msg = format!("{err:#}");
                if msg.contains("loop") {
                    loops.push_front(msg);
                    loops_count += 1;
                } else {
                    warnings.push_front(msg);
                    warnings_count += 1;
                }

                continue;
            }
        };

        if filters.is_match(&path) {
            skipped.push_front(path.to_string());
            skipped_count += 1;
            continue;
        }

        visited.push_front(path);
        visited_count += 1;

        let now = Instant::now();
        if now.duration_since(drawn) < TIME_PER_FRAME {
            continue;
        }

        terminal
            .draw(|frame| {
                use Constraint::*;

                let section = Layout::vertical([Length(1), Min(0)]);
                let rows = Layout::vertical([Length(1), Fill(4), Fill(2), Fill(1), Fill(1)]);
                let [info_area, visited_area, loops_area, warnings_area, skipped_area] =
                    rows.areas(frame.area());

                frame.render_widget(Paragraph::new(format!("Press 'q' to exit")), info_area);

                let [visited_title, visited_content] = section.areas(visited_area);
                let [loops_title, loops_content] = section.areas(loops_area);
                let [warnings_title, warnings_content] = section.areas(warnings_area);
                let [skipped_title, skipped_content] = section.areas(skipped_area);

                frame.render_widget(
                    Block::bordered().title(format!("Visited: {}", visited_count)),
                    visited_title,
                );
                let visited_list = List::new(
                    visited
                        .iter()
                        .take(visited_content.height as usize)
                        .cloned(),
                );
                frame.render_widget(visited_list, visited_content);

                frame.render_widget(
                    Block::bordered().title(format!("Loops: {}", loops_count)),
                    loops_title,
                );
                let loops_list =
                    List::new(loops.iter().take(loops_content.height as usize).cloned());
                frame.render_widget(loops_list, loops_content);

                frame.render_widget(
                    Block::bordered().title(format!("Warnings: {}", warnings_count)),
                    warnings_title,
                );
                let warnings_list = List::new(
                    warnings
                        .iter()
                        .take(warnings_content.height as usize)
                        .cloned(),
                );
                frame.render_widget(warnings_list, warnings_content);

                frame.render_widget(
                    Block::bordered().title(format!("Skipped: {}", skipped_count)),
                    skipped_title,
                );
                let skipped_list = List::new(
                    skipped
                        .iter()
                        .take(skipped_content.height as usize)
                        .cloned(),
                );
                frame.render_widget(skipped_list, skipped_content);
            })
            .context("draw frame")?;

        if handle_tui_events().context("handle TUI events")? {
            break;
        }

        drawn = now;
    }

    Ok(WalkStats {
        skipped: skipped_count,
        warnings: warnings_count,
        loops: loops_count,
        visited: visited_count,
    })
}

fn handle_tui_events() -> std::io::Result<bool> {
    if !event::poll(Duration::from_micros(100))? {
        return Ok(false);
    }

    match event::read()? {
        Event::Key(key) if key.kind == KeyEventKind::Press => match key.code {
            KeyCode::Char('q') => return Ok(true),
            _ => {}
        },
        _ => {}
    }
    Ok(false)
}

#[tracing::instrument(skip(args), fields(root_device_id))]
fn main_logged(root: PathBuf, args: &WalkArgs) -> Result<()> {
    info!("inspecting root: {}", root.display());
    debug!("walk options: {args:?}");

    let root_device_id = util::device_num(&root).wrap_err("get device ID")?;
    tracing::Span::current().record("root_device_id", root_device_id);
    trace!("device_id: {root_device_id}");

    let regexes = args.filters.iter().map(|rx| rx.as_str());
    let filters = RegexSet::new(regexes).wrap_err("parse provided regular expressions as a set")?;
    debug!("parsed user-provided filters: {filters:?}");
    debug!("symbolic link strategy: {:?}", args.symlinks);

    let walker = WalkDir::new(&root)
        .follow_links(matches!(args.symlinks, SymlinkStrategy::Follow))
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
    use stable_eyre::{eyre::Context, Result};
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
    use stable_eyre::{eyre::Context, Result};
    use std::path::Path;
    use winapi_util::{file, Handle};

    pub fn device_num(path: &Path) -> Result<u64> {
        let h = Handle::from_path_any(path)
            .wrap_err_with(|| format!("create system handle for '{}'", path.display()))?;
        file::information(h)
            .wrap_err_with(|| format!("get file information for path '{}'", path.display()))
            .map(|info| info.volume_serial_number())
    }
}
