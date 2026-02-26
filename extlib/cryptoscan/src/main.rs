mod crypto_algorithm;
mod cyclonedx;
mod fips;
mod patterns;
mod scanner;

use clap::{Parser, ValueEnum};
use std::path::PathBuf;

#[derive(Copy, Clone, Debug, ValueEnum)]
enum OutputFormat {
    Json,
    Cyclonedx,
}

#[derive(Copy, Clone, Debug, ValueEnum)]
enum Ecosystem {
    Auto,
    Python,
    Java,
    Go,
    Node,
    Rust,
    Ruby,
    Csharp,
    Php,
    Swift,
    Elixir,
}

impl Ecosystem {
    fn as_str(&self) -> &'static str {
        match self {
            Ecosystem::Auto => "auto",
            Ecosystem::Python => "python",
            Ecosystem::Java => "java",
            Ecosystem::Go => "go",
            Ecosystem::Node => "node",
            Ecosystem::Rust => "rust",
            Ecosystem::Ruby => "ruby",
            Ecosystem::Csharp => "csharp",
            Ecosystem::Php => "php",
            Ecosystem::Swift => "swift",
            Ecosystem::Elixir => "elixir",
        }
    }
}

#[derive(Parser, Debug)]
#[command(name = "cryptoscan", about = "Detect cryptographic algorithm usage in source code")]
struct Cli {
    /// Path to the project directory to scan
    #[arg(short, long)]
    path: PathBuf,

    /// Ecosystem hint
    #[arg(short, long, value_enum, default_value_t = Ecosystem::Auto)]
    ecosystem: Ecosystem,

    /// Output format (json, cyclonedx)
    #[arg(short, long, value_enum, default_value_t = OutputFormat::Cyclonedx)]
    format: OutputFormat,

    /// Only show non-FIPS-compliant findings
    #[arg(long, default_value_t = false)]
    non_fips_only: bool,
}

fn main() {
    let cli = Cli::parse();

    let ecosystems = if matches!(cli.ecosystem, Ecosystem::Auto) {
        scanner::detect_ecosystems(&cli.path)
    } else {
        vec![cli.ecosystem.as_str().to_string()]
    };

    let findings = scanner::scan_project(&cli.path, &ecosystems);

    let findings = if cli.non_fips_only {
        findings
            .into_iter()
            .filter(|f| !f.algorithm.fips_status.is_approved())
            .collect()
    } else {
        findings
    };

    let output = match cli.format {
        OutputFormat::Json => serde_json::to_string_pretty(&findings).expect("Failed to serialize findings"),
        OutputFormat::Cyclonedx => {
            let bom = cyclonedx::to_cyclonedx_bom(&findings);
            serde_json::to_string_pretty(&bom).expect("Failed to serialize CBOM")
        }
    };

    println!("{}", output);
}
