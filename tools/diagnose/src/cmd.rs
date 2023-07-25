use std::{env, path::PathBuf};

use clap::{Parser, Subcommand, ValueEnum};
use getset::{CopyGetters, Getters};
use stable_eyre::{eyre::Context, Result};
use strum::Display;
use tracing::metadata::LevelFilter;
use tracing_subscriber::fmt::format::FmtSpan;

use self::walk::WalkArgs;

pub mod walk;

#[derive(Debug, Parser, Getters, CopyGetters)]
#[clap(version)]
pub struct Opts {
    /// The scan directory to diagnose.
    ///
    /// Provide the same directory here as provided to `fossa analyze`.
    /// `fosssa analyze` implicitly uses the current directory if none provided, this program does the same.
    #[clap(global = true)]
    dir: Option<PathBuf>,

    /// Enable span traces alongside log lines.
    ///
    /// Generally spans correlate to functions; in other words when a function is entered a span is also entered.
    /// There are some exceptions to this (not all functions generate spans, and not all spans are in functions)
    /// but in general this is a reasonable way to think of it for most users.
    #[clap(long, global = true, default_value_t = Span::Full)]
    #[getset(get_copy = "pub")]
    trace_spans: Span,

    /// Set the minimum level for log lines.
    #[clap(long, global = true, default_value_t = Level::Trace)]
    #[getset(get_copy = "pub")]
    trace_level: Level,

    /// The formatter to use for trace data.
    #[clap(short, long, default_value_t = Format::Text, global = true)]
    #[getset(get_copy = "pub")]
    format: Format,

    /// Run a number of subcommands.
    #[clap(subcommand)]
    #[getset(get = "pub")]
    command: Commands,
}

impl Opts {
    pub fn level_filter(&self) -> LevelFilter {
        self.trace_level.into()
    }

    pub fn fmt_span(&self) -> FmtSpan {
        self.trace_spans.into()
    }

    pub fn dir(&self) -> Result<PathBuf> {
        match self.dir {
            Some(ref path) => Ok(path.to_owned()),
            None => env::current_dir().wrap_err("infer current working directory"),
        }
    }
}

/// The log formatting to use.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Parser, ValueEnum, Display)]
pub enum Format {
    /// Output text formatted logs and traces for humans.
    #[strum(serialize = "text")]
    Text,

    /// Output JSON formatted logs and traces for machines.
    #[strum(serialize = "json")]
    Json,
}

/// The minimum level to output.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Parser, ValueEnum, Display)]
pub enum Level {
    /// Emit events that are this level or higher.
    #[strum(serialize = "off")]
    Off,

    /// Emit events that are this level or higher.
    #[strum(serialize = "error")]
    Error,

    /// Emit events that are this level or higher.
    #[strum(serialize = "warn")]
    Warn,

    /// Emit events that are this level or higher.
    #[strum(serialize = "info")]
    Info,

    /// Emit events that are this level or higher.
    #[strum(serialize = "debug")]
    Debug,

    /// Emit events that are this level or higher.
    #[strum(serialize = "trace")]
    Trace,
}

impl From<Level> for LevelFilter {
    fn from(value: Level) -> Self {
        match value {
            Level::Off => LevelFilter::OFF,
            Level::Error => LevelFilter::ERROR,
            Level::Info => LevelFilter::INFO,
            Level::Warn => LevelFilter::WARN,
            Level::Debug => LevelFilter::DEBUG,
            Level::Trace => LevelFilter::TRACE,
        }
    }
}

/// Which parts of span traces to output.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Parser, ValueEnum, Display)]
pub enum Span {
    /// one event when span is created
    #[strum(serialize = "new")]
    New,

    /// one event per enter of a span
    #[strum(serialize = "enter")]
    Enter,

    /// one event per exit of a span
    #[strum(serialize = "exit")]
    Exit,

    /// one event when the span is dropped
    #[strum(serialize = "close")]
    Close,

    /// spans are ignored (this is the default)
    #[strum(serialize = "none")]
    None,

    /// one event per enter/exit of a span
    #[strum(serialize = "active")]
    Active,

    /// events at all points (new, enter, exit, drop)
    #[strum(serialize = "full")]
    Full,
}

impl From<Span> for FmtSpan {
    fn from(val: Span) -> Self {
        match val {
            Span::New => FmtSpan::NEW,
            Span::Enter => FmtSpan::ENTER,
            Span::Exit => FmtSpan::EXIT,
            Span::Close => FmtSpan::CLOSE,
            Span::None => FmtSpan::NONE,
            Span::Active => FmtSpan::ACTIVE,
            Span::Full => FmtSpan::FULL,
        }
    }
}

#[derive(Clone, Debug, Subcommand)]
pub enum Commands {
    /// Diagnose walking the file system.
    Walk(WalkArgs),

    /// Showcase how output is formatted so that users can see what effects formatting arguments have.
    DebugOutputFormat,
}
