use clap::Parser;
use getset::Getters;
use stable_eyre::eyre::{Report, Result};

#[derive(Debug, Parser, Getters)]
#[getset(get = "pub")]
#[clap(version)]
pub struct Subcommand {}

pub fn main(opts: Subcommand) -> Result<()> {
    todo!()
}
