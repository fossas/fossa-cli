use std::{
    fmt::Display,
    io::{self, Read},
    path::PathBuf,
};

use atty::Stream;
use color_eyre::Result;
use colored::Colorize;
use eyre::eyre;
use petgraph::{algo::is_cyclic_directed, prelude::*};
use ptree::print_tree;
use serde::Deserialize;

fn main() -> Result<()> {
    color_eyre::install()?;
    if atty::is(Stream::Stdin) {
        return Err(eyre!(
            "Stream the output of `fossa analyze -o` to this program"
        ));
    }

    let stdin = io::stdin();
    let mut handle = stdin.lock();

    let mut buffer = String::new();
    handle.read_to_string(&mut buffer)?;

    let parsed = serde_json::from_str::<Output>(&buffer)?;
    for Project { path, kind, graph } in parsed.projects.into_iter() {
        let FossaGraph {
            deps,
            direct,
            assocs,
        } = graph;

        println!();
        println!("----- Project: {path:?} ({kind}) -----");
        println!("      {} total deps, {} direct", deps.len(), direct.len());

        let mut graph = Graph::<String, usize>::default();
        let nodes = deps
            .iter()
            .map(|dep| graph.add_node(dep.to_string()))
            .collect::<Vec<_>>();

        for (origin, destinations) in assocs {
            for destination in destinations {
                let edge = (origin, destination);
                let origin = nodes[origin];
                let destination = nodes[destination];

                // Graph doesn't have a method for "will adding this node make it cyclic".
                // Therefore add the edge, then test if cyclic.
                // If it is, remove the edge just added and add a non-cycling node and edge instead.
                //
                // We have to handle cyclic deps this way because actual dependency managers may allow cyclic deps,
                // but our tree renderer will loop forever in the face of cycling deps, so we need to break the cycle.
                let added_edge = graph.add_edge(origin, destination, 1usize);
                if is_cyclic_directed(&graph) {
                    graph.remove_edge(added_edge);

                    let cycle = format!("cycle -> {}", deps[edge.1]);
                    let cycling_node = graph.add_node(cycle);
                    graph.add_edge(origin, cycling_node, 1usize);
                }
            }
        }

        for direct in direct {
            println!();
            let root = nodes[direct];
            print_tree(&(&graph, root))?;
        }
    }

    Ok(())
}

#[derive(Deserialize, Debug)]
struct Output {
    projects: Vec<Project>,
}

#[derive(Deserialize, Debug)]
struct Project {
    #[serde(rename(deserialize = "type"))]
    kind: String,
    graph: FossaGraph,
    path: PathBuf,
}

#[derive(Deserialize, Debug)]
struct FossaGraph {
    assocs: Vec<(usize, Vec<usize>)>,
    direct: Vec<usize>,
    deps: Vec<Dep>,
}

#[derive(Deserialize, Debug, Default, Clone)]
struct Dep {
    #[serde(rename(deserialize = "type"))]
    kind: String,
    name: String,
    version: Option<Version>,
}

impl Display for Dep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Dep {
            name,
            version,
            kind,
        } = self;

        // https://github.com/fossas/fossa-cli/blob/440f7228b871ce8ad84f23011f3fbd5f86cec476/src/Srclib/Converter.hs#L129-L156
        //
        // Colors chosen roughly with the following criteria:
        //
        // Don't use black or white as colors
        //
        // FOSSA-specific kinds (archive, custom, url, user) are magenta
        //
        // Similar colors to each kind's logo colors (e.g. Python is yellow, Ruby is red)
        //
        // If a kind doesn't have a useful representative color (e.g. Perl's
        // logo is black), choose colors that are unlikely to be found in the
        // same analysis output. For example, Java and Haskell dependencies are
        // unlikely to be found together and can share the same color.
        let kind = match kind.as_str() {
            "ArchiveType" => "archive".magenta(),
            "BowerType" => "bower".green(),
            "CarthageType" => "cart".blue(),
            "CargoType" => "cargo".red(),
            "ComposerType" => "comp".yellow(),
            "CondaType" => "conda".yellow(),
            "CpanType" => "cpan".cyan(),
            "CustomType" => "custom".magenta(),
            "GemType" => "gem".red(),
            "GitType" => "git".cyan(),
            "GooglesourceType" => "git".cyan(),
            "GoType" => "go".blue(),
            "HackageType" => "hackage".purple(),
            "HexType" => "hex".red(),
            "LinuxAPK" => "apk".blue(),
            "LinuxDEB" => "deb".red(),
            "LinuxRPM" => "rpm-generic".red(),
            "MavenType" => "mvn".purple(),
            "NodeJSType" => "npm".red(),
            "NuGetType" => "nuget".blue(),
            "PipType" => "pip".yellow(),
            "PodType" => "pod".blue(),
            "RPMType" => "rpm".yellow(),
            "SubprojectType" => "mvn".purple(),
            "URLType" => "url".magenta(),
            "UserType" => "user".magenta(),
            "PubType" => "pub".cyan(),
            "SwiftType" => "swift".red(),
            unknown => panic!("Found dependency \"{name}\" with unknown kind: {unknown}",),
        };

        let version = version
            .as_ref()
            .map(|v| v.to_string())
            .unwrap_or_else(|| "@LATEST".into());
        write!(f, "{name}{version} | {kind}")
    }
}

#[derive(Deserialize, Debug, Default, Clone)]
struct Version {
    #[serde(rename(deserialize = "type"))]
    kind: String,
    value: String,
}

impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Version { kind, value } = self;
        let compat = match kind.as_str() {
            "EQUAL" => "@",
            "COMPATIBLE" => "~",
            "LESSTHAN" => "<",
            "LESSOREQUAL" => "≤",
            "GREATERTHAN" => ">",
            "GREATEROREQUAL" => "≥",
            _ => panic!("Unknown version compatibility: {kind}"),
        };
        write!(f, "{compat}{value}")
    }
}
