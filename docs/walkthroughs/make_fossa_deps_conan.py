"""FOSSA CLI integration script for conan package manager.

Requires:
    - python3 to run this script
    - Conan package manager v2

Usage:
    1. Place this script in same directory as your `conanfile.txt`
    2. Run `python3 make_fossa_deps_conan.py`
        * This will generate `fossa-deps.yml` file from project's dependency graph.
        * You can provide any optional arguments, other than --format from `conan graph info` command
            * For example,
                python3 make_fossa_deps_conan.py -s compiler=gcc

    3. Run `fossa analyze` in the directory where `fossa-deps.yml` is generated.

If you run into any issues with integration, please reach out to us at:
    - https://support.fossa.com/hc/en-us
    
Docs:
    - https://github.com/fossas/fossa-cli
    - https://github.com/fossas/fossa-cli/blob/master/docs/references/files/fossa-deps.md
    - https://github.com/fossas/fossa-cli/blob/master/docs/walkthroughs/conan.md
"""

import sys
import json
import subprocess
import logging
from dataclasses import dataclass
from typing import List, Optional, Tuple
from datetime import datetime
import urllib.parse

logging.basicConfig(
    level=logging.DEBUG,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[logging.StreamHandler()]
)

@dataclass
class FossaVendorDep:
    name: str
    version: str
    path: str

@dataclass
class FossaCustomDepMetadata:
    homepage: Optional[str]
    description: Optional[str]

@dataclass
class FossaCustomDep:
    name: str
    version: str
    license: str
    metadata: Optional[FossaCustomDepMetadata]


@dataclass
class FossaDep:
    vendored_dependencies: List[FossaVendorDep]
    custom_dependencies: List[FossaCustomDep]

    def dump(self):
        comments = [
            "# This is auto-generated fossa-deps file for Conan project.",
            "# This file was generated at: " + datetime.now().isoformat(),
            "# ",
            "# Docs: https://github.com/fossas/fossa-cli/blob/master/docs/walkthroughs/conan.md",
            "# FOSSA: https://fossa.com",
            "# FOSSA Support: https://support.fossa.com/hc/en-us",
            "\n"
        ]

        fossa_deps_yaml = []
        if self.vendored_dependencies:
            fossa_deps_yaml.append("vendored-dependencies:")
            for dep in self.vendored_dependencies:
                fossa_deps_yaml.append(f"- name: {json.dumps(dep.name)}")
                fossa_deps_yaml.append(f"  version: {json.dumps(dep.version)}")
                fossa_deps_yaml.append(f"  path: {json.dumps(dep.path)}")
                fossa_deps_yaml.append("\n")

        if self.custom_dependencies:
            fossa_deps_yaml.append("custom-dependencies:")
            for dep in self.custom_dependencies:
                fossa_deps_yaml.append(f"- name: {json.dumps(dep.name)}")
                fossa_deps_yaml.append(f"  version: {json.dumps(dep.version)}")
                fossa_deps_yaml.append(f"  license: {json.dumps(dep.license)}")

                if dep.metadata is not None and (dep.metadata.homepage is not None or dep.metadata.description is not None):
                    fossa_deps_yaml.append("  meatdata:")
                    if dep.metadata.homepage:
                        fossa_deps_yaml.append(f"    homepage: {json.dumps(dep.metadata.homepage)}")
                    if dep.metadata.description:
                        fossa_deps_yaml.append(f"    description: {json.dumps(dep.metadata.description)}")
                fossa_deps_yaml.append("\n")

        with open('fossa-deps.yml', 'w+') as f:
            f.writelines(s + '\n' for s in comments + fossa_deps_yaml)


def name_version_of(label: str) -> Tuple[str, str]:
    if "/" not in label:
        raise ValueError(f"coult not parse name and version for: {label}")
    name, version = label.split("/", 1)
    return name, version

def license_of(node: dict) -> Optional[str]:
    return node.get("license")

def homepage_of(node: dict) -> Optional[str]:
    candidate = node.get("homepage")
    if not candidate:
        return node.get("url")
    return candidate

def description_of(node: dict) -> Optional[str]:
    return node.get("description")

def mk_fossa_deps(graph):
    if 'root' not in graph or 'nodes' not in graph:
        raise ValueError("root and nodes must exist in the conan graph")

    vendored_deps = []
    custom_deps = []
    for nodeName in graph.get('nodes', []):
        node = graph['nodes'][nodeName]
        label = node.get("label")
        if label.lower() in ["conanfile.txt", "conanfile.py"] or node.get("recipe") == "Consumer":
            logging.info(f"excluding {label} from fossa-deps, as this is a manifest file, not a dependency")
            continue

        if node.get("test", False):
            continue

        # https://docs.conan.io/2.0/tutorial/consuming_packages/cross_building_with_conan.html
        if "build" == node.get("context"):
            logging.info(f"excluding {label} from fossa-deps, as this package as build context, and is build dependency.")
            continue

        pkg_id = node.get("package_id", "none")
        name = node.get("name")
        raw_version = node.get("version")
        if not name or not raw_version:
            name, raw_version = name_version_of(label)
        version_params = urllib.parse.urlencode({'package_id': pkg_id}, doseq=True)
        version = f"{raw_version},{version_params}"

        license = license_of(node)
        homepage = homepage_of(node)
        description = description_of(node)
        src_dir = node.get("source_folder")

        if src_dir is not None:
            logging.info(f"found source code for: {label}, using this as vendored dependency for fossa-deps.")
            vendored_deps.append(FossaVendorDep(name, version, src_dir))
        else:
            logging.info(f"could not find source code in disk for: {label}, using this as vendored dependency for fossa-deps")
            custom_deps.append(FossaCustomDep(name, version, license, FossaCustomDepMetadata(homepage, description)))

    fossa_dep_yml = FossaDep(vendored_deps, custom_deps)
    fossa_dep_yml.dump()

def get_graph(user_args = []):
    logging.info(f"user provided args: {user_args}")
    if ('-f' in user_args or '--format' in user_args):
        raise ValueError("You cannot provide -f | --format opts, as script requires json output from cmd")

    # https://docs.conan.io/2.0/reference/commands/graph/info.html
    graph_cmd = ["conan", "graph", "info", ".", "-f", "json"]

    # https://docs.conan.io/2.0/reference/conanfile/methods/source.html#forced-retrieval-of-sources
    arg_to_download_src = ["-c", "tools.build:download_source=True"]
    
    cmd = graph_cmd + user_args + arg_to_download_src
    logging.info(f"running cmd: {(' ').join(cmd)}")
    result = subprocess.run(cmd, stdout=subprocess.PIPE)

    if (result.returncode != 0):
        exit(result.returncode)
    
    return json.loads(result.stdout)

if __name__ == "__main__":
    graph = get_graph(sys.argv[1:])
    if 'graph' in graph:
        graph = graph['graph']
    mk_fossa_deps(graph)
